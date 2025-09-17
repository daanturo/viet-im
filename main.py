import argparse
import json
import os
import re
import sys
import unicodedata
import urllib.request
from copy import deepcopy
from functools import reduce
from operator import itemgetter
from pathlib import Path
from typing import Self

import json5
from unidecode import unidecode

## * Configs

# ALLOW_FREE_TONE = False

## * Constants

# huyền hỏi ngã sắc nặng
TONE_LIST = ["grave", "hook", "perispomeni", "acute", "dot_below"]

NO_TONE_VOWEL_LETTERS = "aăâeêioôơuưy"
TONE_GRAVE_LETTERS = "àằầèềìòồờùừỳ"
TONE_HOOK_LETTERS = "ảẳẩẻểỉỏổởủửỷ"
TONE_PERISPOMENI_LETTERS = "ãẵẫẽễĩõỗỡũữỹ"
TONE_ACUTE_LETTERS = "áắấéếíóốớúứý"
TONE_DOT_BELOW_LETTERS = "ạặậẹệịọộợụựỵ"

TONAL_VOWEL_LETTERS = (
    TONE_GRAVE_LETTERS
    + TONE_HOOK_LETTERS
    + TONE_PERISPOMENI_LETTERS
    + TONE_ACUTE_LETTERS
    + TONE_DOT_BELOW_LETTERS
)
# (1+2)*6 + 5*(2+3)*6 = 168 after NFD decompose
VOWEL_LETTERS = NO_TONE_VOWEL_LETTERS + TONAL_VOWEL_LETTERS
CONSONANT_LETTERS = "bcdfgjklmnpqstvxzhrw" + "đ"
COMBINATIVE_CONSONANTS_WITH_VOWEL_LETTERS = ("gi", "qu")

ALPHABET_DIACRITIC_LETTERS = "ăâêôơư"
# NOTE: "đ" & "Đ" (Letter D with stroke, U+0111 & U+0110) aren't combining characters
ALPHABET_DIACRITIC_TABLE = {
    "circumflex": {
        "code_point": 0x0302,
        "letters": "âêô",
        "im_vni": "6",
        "im_telex": "aeo",
    },
    "breve": {
        "code_point": 0x0306,
        "letters": "ă",
        "im_vni": "8",
        "im_telex": "w",
    },
    "horn": {
        "code_point": 0x031B,
        "letters": "ơư",
        "im_vni": "7",
        "im_telex": "w",
    },
}
TONE_TABLE = {
    "unmarked": {
        "code_point": None,
        "name": "ngang",
        "im_vni": "0",
        "im_telex": "z",
    },
    "acute": {
        "code_point": 0x0301,
        "name": "sắc",
        "im_vni": "1",
        "im_telex": "s",
    },
    "grave": {
        "code_point": 0x0300,
        "name": "huyền",
        "im_vni": "2",
        "im_telex": "f",
    },
    "hook": {
        "code_point": 0x0309,
        "name": "hỏi",
        "im_vni": "3",
        "im_telex": "r",
    },
    "perispomeni": {
        "code_point": 0x0303,
        "name": "ngã",
        "im_vni": "4",
        "im_telex": "x",
    },
    "dot_below": {
        "code_point": 0x0323,
        "name": "nặng",
        "im_vni": "5",
        "im_telex": "j",
    },
}

## * Helpers


def truthy(arg) -> False:
    # python: (0 == False) : True
    return (not ((False == arg) and isinstance(arg, bool))) and (not (arg is None))


def some_of(*seq) -> bool:
    """Return the first "truthy" value in ARGS, if none of them return the last
    one.  Consider values other than False and None truthy, such as 0, [], empty
    string (\"\"), etc."""
    for x in seq:
        if truthy(x):
            return x
    return seq[-1]


def some(fn, seq):
    for x in seq:
        y = fn(x)
        if truthy(y):
            return y
    return None


def remove_string_all_diacritics(string: str) -> str:
    # Need external solution anyway, unicodedata.normalize("NFD", string) doesn't convert "đ"
    return unidecode(string)


def remove_string_alphabet_diacritics(string: str):
    "Keep tone."
    removing = [chr(v["code_point"]) for v in ALPHABET_DIACRITIC_TABLE.values()]
    keeping = [
        char for char in unicodedata.normalize("NFD", string) if not (char in removing)
    ]
    return unicodedata.normalize("NFC", ("".join(keeping).replace("đ", "d")))


def remove_string_tone_diacritics(string: str):
    "Keep alphabet diacritic."
    removing = [chr(v["code_point"]) for v in TONE_TABLE.values() if v["code_point"]]
    keeping = [
        char for char in unicodedata.normalize("NFD", string) if not (char in removing)
    ]
    return unicodedata.normalize("NFC", ("".join(keeping)))


def get_string_tone(string: str):
    "Expect an NFC normalized string."
    table = {chr(v["code_point"]): k for k, v in TONE_TABLE.items() if v["code_point"]}
    for i in range(len(string)):
        chars = unicodedata.normalize("NFD", string[i])
        for char in chars:
            tone_name = table.get(char)
            if tone_name:
                return {
                    "tone": tone_name,
                    "position": i,
                    "code_point": TONE_TABLE[tone_name]["code_point"],
                }
    return None


def get_string_tone_potential_position(word: str):
    found = get_string_tone(word)
    if found:
        return found["position"]
    tone_pos = (get_preliminary_table()).get(word, dict()).get("tone_position")
    if tone_pos is not None:
        return tone_pos
    components = parse_word_components(word)
    vowel_part = components[2]
    vowel_tone_pos = (
        (get_preliminary_table()).get(vowel_part, dict()).get("tone_position")
    )
    if vowel_tone_pos is not None:
        return len(components[0] or "") + vowel_tone_pos


def get_string_alphabet_diacritic(string: str):
    "Expect an NFC normalized string."
    table = {chr(v["code_point"]): k for k, v in ALPHABET_DIACRITIC_TABLE.items()}
    for i in range(len(string)):
        chars = unicodedata.normalize("NFD", string[i])
        for char in chars:
            if table.get(char):
                return {"alphabet_diacritic": table.get(char), "position": i}
    return None


_consonant_re = (
    "|".join(COMBINATIVE_CONSONANTS_WITH_VOWEL_LETTERS) + "|" + f"[{CONSONANT_LETTERS}]+"
)
VIETNAMESE_WORD_REGEX = (
    rf"^({_consonant_re})?(([{VOWEL_LETTERS}]+)([{CONSONANT_LETTERS}]+)?)$"
)


def parse_word_components(word):
    match = re.match(VIETNAMESE_WORD_REGEX, word)
    if match:
        return match.groups()


## * Combinative vowels


# EXCEPTION: ?
def is_invalid_tone(tone_name, suffix_consonant_part):
    return tone_name in ["grave", "perispomeni", "hook"] and suffix_consonant_part in [
        "c",
        "ch",
        "p",
        "t",
    ]


def _get_words_from_dictionary():
    dict_filename = "vi-DauMoi.dic"
    if not os.path.exists(dict_filename):
        urllib.request.urlretrieve(
            "https://github.com/1ec5/hunspell-vi/raw/refs/heads/main/dictionaries/vi-DauMoi.dic",
            filename=dict_filename,
        )
    with open(dict_filename) as f:
        words = f.read().splitlines()
    with open("exclude.dic") as f:
        exclude_words = f.read().splitlines()
    # Ignore words with upper case letter and numbers
    words = [
        word
        for word in words
        if (
            (not (any(map(lambda c: c.isupper(), word)) or re.search(r"[0-9]", word)))
            and (word not in exclude_words)
        )
    ]
    return words


def _parse_words(words):
    prefix_consonants = set()
    rhymes = set()
    combin_vowels = set()
    suffix_consonants = set()
    for word in words:
        groups = parse_word_components(word)
        # "basoi"
        if groups is None:
            continue
        assert groups[1]
        if groups[0]:
            prefix_consonants.add(groups[0])
        rhymes.add(groups[1])
        if groups[2]:
            combin_vowels.add(groups[2])
        if groups[3]:
            suffix_consonants.add(groups[3])
    return {
        "prefix_consonants": sorted(prefix_consonants),
        "rhymes": sorted(rhymes),
        "combinative_vowels": sorted(combin_vowels),
        "suffix_consonants": sorted(suffix_consonants),
    }


def get_words_and_rhymes(words=None):
    words = words or _get_words_from_dictionary()
    word_set = set(words)

    word_set_1 = set(
        word
        for word in word_set
        if not word.startswith(COMBINATIVE_CONSONANTS_WITH_VOWEL_LETTERS)
    )

    # Strip special "phụ âm đầu"s that include a vowel
    word_set_gi_qu = set(
        reduce(
            lambda w, p: w.removeprefix(p),
            COMBINATIVE_CONSONANTS_WITH_VOWEL_LETTERS,
            word,
        )
        for word in word_set
        if word.startswith(COMBINATIVE_CONSONANTS_WITH_VOWEL_LETTERS)
    )

    set_1_components = _parse_words(word_set_1)

    set_1_ascii_rhymes = set(
        remove_string_all_diacritics(rhyme) for rhyme in set_1_components["rhymes"]
    )
    filterd_set_gi_qu = set(
        w2
        for w2 in word_set_gi_qu
        if remove_string_all_diacritics(w2) in set_1_ascii_rhymes
    )
    # word_set_gi_qu - filterd_set_gi_qu == {'', 'ýt', 'ýnh', 'p', 'ỷnh', 'ỳnh', 'ỵt'}

    # An insignificantly little inefficient to do parsing again, but simple
    return _parse_words(word_set_1 | filterd_set_gi_qu)


def make_preliminary_rhyme_table(rhymes, combin_vowels):
    rhyme_table = dict()
    for rhyme in rhymes + combin_vowels:
        tone_info = get_string_tone(rhyme)
        rhyme_no_tone = remove_string_tone_diacritics(rhyme)
        rhyme_ascii = remove_string_all_diacritics(rhyme)
        if rhyme_table.get(rhyme_no_tone) is None:
            rhyme_table[rhyme_no_tone] = dict()
        if rhyme_table.get(rhyme_ascii) is None:
            rhyme_table[rhyme_ascii] = dict()
        tone_pos = some_of(
            (tone_info and tone_info.get("position")),
            rhyme_table[rhyme_no_tone].get("tone_position"),
        )
        # Mark tone position of non tonal rhymes using their tonal variants
        if tone_pos is not None:
            rhyme_table[rhyme_no_tone]["tone_position"] = tone_pos
            rhyme_table[rhyme_ascii]["tone_position"] = tone_pos
    for rhyme, tone_pos in json5.loads(
        Path("./manual-tone-positions.jsonc").read_text("utf8")
    ).items():
        for s in [rhyme, remove_string_all_diacritics(rhyme)]:
            if not s in rhyme_table:
                rhyme_table[s] = dict()
            rhyme_table[s]["tone_position"] = tone_pos
    # Some combinative vowels are complete rhymes, note the order
    # for s in combin_vowels:
    # rhyme_table[remove_string_tone_diacritics(s)]["is_complete_rhyme"] = False
    # for s in rhymes:
    # rhyme_table[remove_string_tone_diacritics(s)]["is_complete_rhyme"] = True
    for k, v in rhyme_table.items():
        assert isinstance(v["tone_position"], int)
    return rhyme_table


preliminary_table_file = "generated-preliminary-table.json"
_preliminary_rhyme_table = None
# try:
#     with open(preliminary_table_file) as f:
#         _preliminary_rhyme_table = json5.load(f)
# except Exception: pass


def _init_preliminary_table(rhymes, combin_vowels):
    rhyme_table = make_preliminary_rhyme_table(rhymes, combin_vowels)
    rhyme_table_no_tone = {
        k: v for (k, v) in rhyme_table.items() if not get_string_tone(k)
    }
    return rhyme_table_no_tone


def get_preliminary_table():
    global _preliminary_rhyme_table
    if _preliminary_rhyme_table is None:
        hmap = get_words_and_rhymes()
        _preliminary_rhyme_table = _init_preliminary_table(
            hmap["rhymes"], hmap["combinative_vowels"]
        )
        if True:
            with open(preliminary_table_file, "w", encoding="utf8") as f:
                json.dump(_preliminary_rhyme_table, f, ensure_ascii=False, indent=2)
    return _preliminary_rhyme_table


class Rhyme:
    """The vowel part is either elemental or combinative.  In a vowel, for each
    of alphabet diacritic and tone, there is only at most one class."""

    _text: str
    _alphabet_diacritic_name = None
    _prefix_consonant_part = ""
    _vowel_part = ""
    _suffix_consonant_part = ""
    _tone_code_point_as_str = ""
    _marked_tone_name = None

    tone_position = None
    ascii = ""

    no_alphabet_diacritic = None
    breve = None
    circumflex = None
    horn = None

    unmarked = None
    grave = None
    hook = None
    perispomeni = None
    acute = None
    dot_below = None

    # is_complete_rhyme = False

    def __init__(self, text):
        text = unicodedata.normalize("NFC", text)
        self._text = text

        match_groups = parse_word_components(text)
        assert match_groups[2]
        self._prefix_consonant_part = match_groups[0] or ""
        self._vowel_part = match_groups[2]
        self._suffix_consonant_part = match_groups[3] or ""

        found = get_string_tone(text)
        if not found:
            self._tone_code_point_as_str = ""
            self.tone_position = get_string_tone_potential_position(text)
            if len(self._vowel_part) == 1 and self.tone_position is None:
                self.tone_position = len(self._prefix_consonant_part)
            self._marked_tone_name = None
        else:
            self.tone_position = found["position"]
            self._tone_code_point_as_str = chr(found["code_point"])
            self._marked_tone_name = found["tone"]

        found = get_string_alphabet_diacritic(text)
        if not found:
            self._alphabet_diacritic_name = None
        else:
            self._alphabet_diacritic_name = found["alphabet_diacritic"]

        self.ascii = remove_string_all_diacritics(text)
        self.no_alphabet_diacritic = remove_string_alphabet_diacritics(text)
        self.unmarked = remove_string_tone_diacritics(text)

        self.breve = self.with_alphabet_diacritic("breve")
        self.circumflex = self.with_alphabet_diacritic("circumflex")
        self.horn = self.with_alphabet_diacritic("horn")

        self.grave = self.with_tone("grave")
        self.hook = self.with_tone("hook")
        self.perispomeni = self.with_tone("perispomeni")
        self.acute = self.with_tone("acute")
        self.dot_below = self.with_tone("dot_below")

        # self.is_complete_rhyme = (get_preliminary_table())[self.unmarked]["is_complete_rhyme"]

    def without_alphabet_diacritic(self) -> str:
        txt = "".join(remove_string_alphabet_diacritics(c) for c in self._text)
        return txt

    def without_tone(self) -> str:
        txt = "".join(remove_string_tone_diacritics(c) for c in self._text)
        return txt

    def with_alphabet_diacritic(self, apb_diacriatic: str) -> str:
        ascii_vowels = remove_string_all_diacritics(self._vowel_part)
        new_vowel_part_char_list = []
        for i in range(len(ascii_vowels)):
            char_as_ascii = ascii_vowels[i]
            new_char = unicodedata.normalize(
                "NFC",
                char_as_ascii
                + chr(ALPHABET_DIACRITIC_TABLE[apb_diacriatic]["code_point"]),
            )
            # a duplicate ASCII vowel in a rhyme don't get have diacritics ("ưu", "ươu"), "oo" can have tones but not alphabetic diacritics
            if char_as_ascii in ascii_vowels[:i]:
                new_vowel_part_char_list += [char_as_ascii]
            # doesn't exist in the alphabet, return old char with tone stripped,
            # since changing alphabetic diacritic may change the tone position
            # ("úa"->"uấ"), query and add it back later
            elif new_char not in VOWEL_LETTERS:
                new_vowel_part_char_list += [
                    remove_string_tone_diacritics(self._vowel_part[i])
                ]
            else:
                new_vowel_part_char_list += [new_char]
        new_rhyme = "".join(new_vowel_part_char_list) + self._suffix_consonant_part
        if not (
            (len(new_rhyme) == 1 and new_rhyme in VOWEL_LETTERS)
            or ((get_preliminary_table()).get(new_rhyme))
        ):
            return self._text
        if self._marked_tone_name:
            new_tone_pos = get_preliminary_table()[new_rhyme]["tone_position"]
            # + 1: combining code point needs to be AFTER the letter
            new_rhyme = (
                new_rhyme[: new_tone_pos + 1]
                + self._tone_code_point_as_str
                + new_rhyme[new_tone_pos + 1 :]
            )
            new_rhyme = unicodedata.normalize("NFC", new_rhyme)
        retval = self._prefix_consonant_part + new_rhyme
        return retval

    def with_tone(self, tone: str) -> str:
        if self.tone_position is None or is_invalid_tone(
            tone, self._suffix_consonant_part
        ):
            return self._text
        tone_pos = self.tone_position
        old_char_with_tone = self._text[self.tone_position]
        new_char_with_tone = remove_string_tone_diacritics(old_char_with_tone) + chr(
            TONE_TABLE[tone]["code_point"]
        )
        new_char_with_tone = unicodedata.normalize("NFC", new_char_with_tone)
        unmarked_retval = (
            self._text[:tone_pos] + new_char_with_tone + self._text[tone_pos + 1 :]
        )
        return unmarked_retval


## * Process


# EXCEPTION: ?
def _im_special_rules_for_gi_qu(im_name):
    ret_table = dict()
    prelim_table = get_preliminary_table()

    # "u" in "qu" is unresponsive to combining, unlike lone "u"
    horn_u_trigger = ALPHABET_DIACRITIC_TABLE["horn"][im_name]
    ret_table["qu" + horn_u_trigger] = "qu" + horn_u_trigger
    for char in [v[im_name] for v in TONE_TABLE.values() if v["code_point"]]:
        ret_table["qu" + char] = "qu" + char

    # "qu" interactions with rhymes that start with "u":
    # - Tone at "u" (0), when add tone:
    #   + Rhyme has another vowel and valid if initial "u" stripped: use that stripped rhyme
    #   + Else: unresponsive
    # - Initial "u" has a diacritic (="ư"): when add diacritic: unresponsive
    for rhyme in prelim_table.keys():
        if rhyme[0] in "u" and prelim_table[rhyme]["tone_position"] == 0:
            for tone_name in [k for (k, v) in TONE_TABLE.items() if v["code_point"]]:
                rhyme_qu = rhyme[1:]
                key = "qu" + rhyme_qu + TONE_TABLE[tone_name][im_name]
                if rhyme_qu in prelim_table:
                    rhyme_qu_tonal = Rhyme(rhyme_qu).with_tone(tone_name)
                    ret_table[key] = "qu" + rhyme_qu_tonal
                else:
                    ret_table[key] = key
        if rhyme[0] in "ư":
            s = "q" + remove_string_alphabet_diacritics(rhyme) + horn_u_trigger
            ret_table[s] = s

    # "gi"+tone+vowel, tone is repositioned -> "gi"+vowel+tone, like
    # "gía"->"giá", but I decide not to do this, since words like "gía", "gìu",
    # etc. while not in dictionary, there are valid vocal spellings that match
    # them "zía", "zìu", etc.
    for tone_name, tone_data in [
        (tone_name, tone_data)
        for (tone_name, tone_data) in TONE_TABLE.items()
        if tone_data["code_point"]
    ]:
        tone_cp = tone_data["code_point"]

        # for new_char in filter(lambda x: x not in "iy", NO_TONE_VOWEL_LETTERS):
        #       key = unicodedata.normalize("NFC", "gi" + chr(tone_cp) + new_char)
        #       val = unicodedata.normalize("NFC", "gi" + new_char + chr(tone_cp))
        #       ret_table[key] = val

        # Don't let "i" of "gi" steal the correct tone
        for rhyme in prelim_table.keys():
            if (
                re.search(rf"^i[{VOWEL_LETTERS}]", rhyme)
                and prelim_table[rhyme]["tone_position"] == 0
            ):
                ret_table["gi" + rhyme[1:] + tone_data[im_name]] = unicodedata.normalize(
                    "NFC", "gi" + Rhyme(rhyme[1:]).with_tone(tone_name)
                )

    return ret_table


def _make_im_case(im_name: str, rhyme_table: dict, pre: str, rhyme: str, rules: dict):
    diacritic_table = ALPHABET_DIACRITIC_TABLE | TONE_TABLE
    rhyme_str = deepcopy(rhyme)
    orig_rhyme_ascii = remove_string_all_diacritics(rhyme_str)
    if pre in COMBINATIVE_CONSONANTS_WITH_VOWEL_LETTERS:
        pre_and_rhyme_share_char = pre[-1] == remove_string_all_diacritics(rhyme_str[0])
        rhyme_has_2_or_more_vowels = re.search(
            rf"^[{VOWEL_LETTERS}][{VOWEL_LETTERS}]", rhyme_str
        )
        match None:
            # quu, quuu, quou, quuou are invalid
            case _ if pre == "qu" and orig_rhyme_ascii in ["uu", "uou"]:
                return None
            # If prefix combinative consonant and rhyme shares a character
            # like qu+ua, gi+ia, qu+uan etc., rhyme has 2 or more vowel
            # letters, treat those specially: tones are put on the rhyme
            # instead
            case _ if pre_and_rhyme_share_char and rhyme_has_2_or_more_vowels:
                rhyme_str = rhyme_str[1:]
            # If prefix combinative consonant but not share: skip this rule,
            # this isn't special
            case _:
                return None
    rhyme_obj = Rhyme(rhyme_str)
    for diacritic_name in diacritic_table.keys():
        trigger_chars = diacritic_table[diacritic_name][im_name]
        for trigger in trigger_chars:
            key = pre + rhyme_str + trigger
            diact_added_result = rhyme_table.get(rhyme_str, dict()).get(diacritic_name)
            # Invalid like qu+ou
            if diact_added_result is None:
                return None
            match None:
                # telex "auo" -> "auo", not "âu"
                case _ if (len(trigger_chars) > 1) and (
                    not (remove_string_all_diacritics(trigger) in rhyme_obj.ascii)
                ):
                    pass
                case _ if diact_added_result != rhyme_str:
                    rules[key] = pre + diact_added_result
                case _ if diacritic_name == rhyme_obj._marked_tone_name:
                    rules[key] = pre + rhyme_obj.without_tone() + trigger
                case _ if diacritic_name == rhyme_obj._alphabet_diacritic_name:
                    rules[key] = pre + rhyme_obj.without_alphabet_diacritic() + trigger
    return rules


def _make_im(
    im_name: str,
    rhyme_table: dict,
    initial_rules: dict,
):
    rules = initial_rules
    d_trigger = {"im_vni": "9", "im_telex": "d"}[im_name]
    for rhyme in rhyme_table.keys():
        if rhyme[0] in VOWEL_LETTERS:
            rules["d" + rhyme + d_trigger] = "đ" + rhyme
            rules["đ" + rhyme + d_trigger] = "d" + rhyme + d_trigger
        for pre in [""]:
            res = _make_im_case(im_name, rhyme_table, pre, rhyme, rules)
            if res:
                rules = res
    rules |= _im_special_rules_for_gi_qu(im_name)
    rules = dict(sorted(rules.items()))
    return rules


def make_im_vni(rhyme_table=None):
    rhyme_table = rhyme_table or make_full_rhyme_table()
    rules = {
        "d9": "đ",
        "đ9": "d9",
        "dd9": "dd9",  # is this necessary?
    }
    rules = _make_im("im_vni", rhyme_table, rules)
    with open("generated-im-vni.json", "w") as f:
        json.dump(rules, f, ensure_ascii=False, indent=2)
    return rules


def make_im_telex(rhyme_table=None):
    rhyme_table = rhyme_table or make_full_rhyme_table()
    """Currently not supported: repositioning tones ("ghìê": "ghiề", etc.), it's
    weird, why not type tones at the end of word?"""
    rules = {
        "dd": "đ",
        "đd": "dd",
        "ddd": "ddd",
        "w": "ư",
    }
    rules = _make_im("im_telex", rhyme_table, rules)
    with open("generated-im-telex.json", "w") as f:
        json.dump(rules, f, ensure_ascii=False, indent=2)
    return rules


def make_full_rhyme_table():
    hmap = get_words_and_rhymes()
    valid_rhyme_list = sorted(set(hmap["rhymes"] + hmap["combinative_vowels"]))

    base_rhymes = set(
        base_variant
        for valid_rhyme in valid_rhyme_list
        for base_variant in [
            valid_rhyme,
            remove_string_all_diacritics(valid_rhyme),
            remove_string_alphabet_diacritics(valid_rhyme),
            remove_string_tone_diacritics(valid_rhyme),
        ]
    )

    rhyme_objs = [Rhyme(s) for s in base_rhymes] + [
        Rhyme(Rhyme(Rhyme(base_variant).with_alphabet_diacritic(apb_dct)).with_tone(tone))
        for base_variant in base_rhymes
        for apb_dct in ALPHABET_DIACRITIC_TABLE.keys()
        # A bit of future-proofing: consider rhyme+tone combinations not in dictionary (yet)
        for tone in TONE_TABLE.keys()
        if TONE_TABLE[tone]["code_point"]
    ]

    # Make public properties dict keys
    rhyme_table = {
        ro._text: {k: v for k, v in ro.__dict__.items() if not k.startswith("_")}
        for ro in rhyme_objs
    }
    rhyme_table = dict(sorted(rhyme_table.items()))
    return rhyme_table


def main():
    rhyme_table = make_full_rhyme_table()
    with open("generated-rhyme-table.json", "w") as f:
        json.dump(rhyme_table, f, ensure_ascii=False, indent=2)
    make_im_vni(rhyme_table)
    make_im_telex(rhyme_table)


if __name__ == "__main__":
    main()
