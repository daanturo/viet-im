import argparse
import json
import os
import re
import sys
import unicodedata
import urllib
from copy import deepcopy
from functools import reduce
from operator import itemgetter
from typing import Self

import json5
from unidecode import unidecode

## * Constants

# huyền hỏi ngã sắc nặng
TONE_LIST = ["grave", "hook", "perispomeni", "acute", "dot_below"]

NO_TONAL_VOWEL_LETTERS = "aăâeêioôơuưy"
TONE_GRAVE_LETTERS = "àằầèềìòồờùừỳ"
TONE_HOOK_LETTERS = "ảẳẩẻểỉỏổởủửỷ"
TONE_PERISPOMENI_LETTERS = "ãẵẫẽễĩõỗỡũữỹ"
TONE_ACUTE_LETTERS = "áắấéếíóốớúứý"
TONE_DOT_BELOW_LETTERS = "ạặậẹệịọộợụựỵ"

TONAL_VOWEL_LETTERS = (
    TONE_GRAVE_LETTERS + TONE_HOOK_LETTERS + TONE_PERISPOMENI_LETTERS + TONE_ACUTE_LETTERS + TONE_DOT_BELOW_LETTERS
)
# (1+2)*6 + 5*(2+3)*6 = 168 after NFD decompose
VOWEL_LETTERS = NO_TONAL_VOWEL_LETTERS + TONAL_VOWEL_LETTERS
CONSONANT_LETTERS = "bcdfgjklmnpqstvxzhrw" + "đ"
COMBINATIVE_CONSONANTS_WITH_VOWEL_LETTERS = ["gi", "qu"]

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


def some_truthy(*args) -> bool:
    """Return the first "truthy" value in ARGS, if none of them return the last
    one.  Consider values other than False and None truthy, such as 0, [], empty
    string (\"\"), etc."""
    for x in args:
        # python: (0 == False) : True
        if (not ((False == x) and isinstance(x, bool))) and (not (x is None)):
            return x
    return args[-1]


def remove_string_all_diacritics(string: str) -> str:
    # Need external solution anyway, unicodedata.normalize("NFD", string) doesn't convert "đ"
    return unidecode(string)


def remove_string_alphabet_diacritics(string: str):
    "Keep tone."
    removing = [chr(v["code_point"]) for v in ALPHABET_DIACRITIC_TABLE.values()]
    keeping = [char for char in unicodedata.normalize("NFD", string) if not (char in removing)]
    return unicodedata.normalize("NFC", ("".join(keeping).replace("đ", "d")))


def remove_string_tone_diacritics(string: str):
    "Keep alphabet diacritic."
    removing = [chr(v["code_point"]) for v in TONE_TABLE.values() if v["code_point"]]
    keeping = [char for char in unicodedata.normalize("NFD", string) if not (char in removing)]
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


def get_string_alphabet_diacritic(string: str):
    "Expect an NFC normalized string."
    table = {chr(v["code_point"]): k for k, v in ALPHABET_DIACRITIC_TABLE.items()}
    for i in range(len(string)):
        chars = unicodedata.normalize("NFD", string[i])
        for char in chars:
            if table.get(char):
                return {"alphabet_diacritic": table.get(char), "position": i}
    return None


### * Combinative vowels


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
            (not (any(map(lambda c: c.isupper(), word)) or re.search(r"[0-9]", word))) and (not (word in exclude_words))
        )
    ]
    return words


def get_words_and_rhymes(words=None):
    words = words or _get_words_from_dictionary()
    rhyme_list = words
    # Strip special "phụ âm đầu"s that include a vowel
    rhyme_list = [
        reduce(
            lambda w, p: w.removeprefix(p),
            COMBINATIVE_CONSONANTS_WITH_VOWEL_LETTERS,
            word,
        )
        for word in rhyme_list
    ]

    prefix_consonants = set(COMBINATIVE_CONSONANTS_WITH_VOWEL_LETTERS)
    rhymes = set()
    combin_vowels = set()
    suffix_consonants = set()

    for rhyme in rhyme_list:
        m = re.match(
            rf"^([{CONSONANT_LETTERS}]+)?(([{VOWEL_LETTERS}]+)([{CONSONANT_LETTERS}]+)?)$",
            rhyme,
        )
        if m:
            if m.group(1):
                prefix_consonants.add(m.group(1))
            rhymes.add(m.group(2))
            if m.group(3):
                combin_vowels.add(m.group(3))
            if m.group(4):
                suffix_consonants.add(m.group(4))

    return {
        "words": sorted(words),
        "prefix_consonants": sorted(prefix_consonants),
        "rhymes": sorted(rhymes),
        "combinative_vowels": sorted(combin_vowels),
        "suffix_consonants": sorted(suffix_consonants),
    }


def make_preliminary_rhyme_table(rhymes, combin_vowels):
    with open("./manual-tone-positions.json", encoding="utf-8") as f:
        manual_tone_positions = json5.load(f)
    rhyme_table = dict()
    for rhyme in rhymes + combin_vowels:
        rhyme_no_tone = remove_string_tone_diacritics(rhyme)
        tone_info = get_string_tone(rhyme)
        manual_tone_pos = manual_tone_positions.get(rhyme_no_tone)
        if rhyme_table.get(rhyme_no_tone) is None:
            rhyme_table[rhyme_no_tone] = dict()
        # Mark tone position of non tonal rhymes using their tonal variants
        if manual_tone_pos is not None:
            rhyme_table[rhyme_no_tone]["tone_position"] = manual_tone_pos
        elif rhyme_table[rhyme_no_tone].get("tone_position") is None and tone_info:
            rhyme_table[rhyme_no_tone]["tone_position"] = tone_info["position"]
    # Some combinative vowels are complete rhymes, note the order
    for s in combin_vowels:
        rhyme_table[remove_string_tone_diacritics(s)]["is_complete_rhyme"] = False
    for s in rhymes:
        rhyme_table[remove_string_tone_diacritics(s)]["is_complete_rhyme"] = True
    return rhyme_table


preliminary_table_file = "generated-preliminary-table.json"
_preliminary_rhyme_table = None
# try:
#     with open(preliminary_table_file) as f:
#         _preliminary_rhyme_table = json5.load(f)
# except Exception: pass


def _init_preliminary_table(rhymes, combin_vowels):
    rhyme_table = make_preliminary_rhyme_table(rhymes, combin_vowels)
    rhyme_table_no_tone = {k: v for (k, v) in rhyme_table.items() if not get_string_tone(k)}
    return rhyme_table_no_tone


def get_preliminary_table():
    global _preliminary_rhyme_table
    if _preliminary_rhyme_table is None:
        hmap = get_words_and_rhymes()
        _preliminary_rhyme_table = _init_preliminary_table(hmap["rhymes"], hmap["combinative_vowels"])
        if True:
            with open(preliminary_table_file, "w", encoding="utf8") as f:
                json.dump(_preliminary_rhyme_table, f, ensure_ascii=False, indent=2)
    return _preliminary_rhyme_table


class Rhyme:
    """The vowel part is either elemental or combinative.  In a vowel, for each
    of alphabet diacritic and tone, there is only at most one class."""

    _text: str
    _alphabet_diacritic_name = None
    _vowel_part = ""
    _suffix_consonant_part = ""
    _tone_string = ""
    _tone_name = None

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

    is_complete_rhyme = False

    def __init__(self, text):
        text = unicodedata.normalize("NFC", text)
        self._text = text

        m = re.match(
            rf"^([{VOWEL_LETTERS}]+)([{CONSONANT_LETTERS}]+)?$",
            text,
        )
        assert m.group(1)
        self._vowel_part = m.group(1)
        self._suffix_consonant_part = m.group(2) or ""

        found = get_string_tone(text)
        if not found:
            self._tone_string = ""
            self.tone_position = some_truthy(
                (get_preliminary_table()).get(text, dict()).get("tone_position", None),
                (get_preliminary_table()).get(self._vowel_part, dict()).get("tone_position", None),
            )
        else:
            self.tone_position = found["position"]
            self._tone_string = chr(found["code_point"])
            self._tone_name = found["tone"]
        if len(self._vowel_part) == 1:
            self.tone_position = 0

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

        self.is_complete_rhyme = (get_preliminary_table())[self.unmarked]["is_complete_rhyme"]

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
                char_as_ascii + chr(ALPHABET_DIACRITIC_TABLE[apb_diacriatic]["code_point"]),
            )
            # a duplicate ASCII vowel in a rhyme don't get have diacritics ("ưu", "ươu"), "oo" can have tones but not alphabetic diacritics
            if char_as_ascii in ascii_vowels[:i]:
                new_vowel_part_char_list += [char_as_ascii]
            # doesn't exist in the alphabet
            elif new_char not in VOWEL_LETTERS:
                new_vowel_part_char_list += [self._vowel_part[i]]
            else:
                new_vowel_part_char_list += [new_char]
                # If at tone position (that only appears on vowels), add the tone code point to be normalized later
                if self._tone_name and self.tone_position == i:
                    new_vowel_part_char_list += [self._tone_string]
        retval = unicodedata.normalize("NFC", "".join(new_vowel_part_char_list))
        retval = retval + self._suffix_consonant_part
        if not (
            (len(retval) == 1 and retval in VOWEL_LETTERS)
            # This check must be consistent with get_preliminary_table: whether the table includes suffix consonants
            or (get_preliminary_table()).get(remove_string_tone_diacritics(retval))
        ):
            return self._text
        return retval

    def with_tone(self, tone: str) -> str:
        if self.tone_position is None:
            return self._text
        tone_pos = self.tone_position
        old_char_with_tone = self._text[self.tone_position]
        new_char_with_tone = remove_string_tone_diacritics(old_char_with_tone) + chr(TONE_TABLE[tone]["code_point"])
        new_char_with_tone = unicodedata.normalize("NFC", new_char_with_tone)
        retval = self._text[:tone_pos] + new_char_with_tone + self._text[tone_pos + 1 :]
        return retval


## * Process


def make_im_vni(rhyme_table):
    diacritic_table = ALPHABET_DIACRITIC_TABLE | TONE_TABLE
    rules = {
        "d9": "đ",
        "đ9": "d9",
    }
    for pre in ["", "d", "đ"]:
        for rhyme in rhyme_table.keys():
            rhyme_obj = Rhyme(rhyme)
            for diacritic_name in diacritic_table.keys():
                trigger = diacritic_table[diacritic_name]["im_vni"]
                key = pre + rhyme + trigger
                diact_added_result = rhyme_table[rhyme][diacritic_name]
                match None:
                    # A changing case: proceed as stated
                    case _ if diact_added_result != rhyme:
                        rules[key] = pre + diact_added_result
                    # Tone is repeated: remove it and add trigger as-is
                    case _ if diacritic_name == rhyme_obj._tone_name:
                        rules[key] = pre + rhyme_obj.without_tone() + trigger
                    # Alphabet diacritic is repeated: remove it and add trigger as-is
                    case _ if diacritic_name == rhyme_obj._alphabet_diacritic_name:
                        rules[key] = pre + rhyme_obj.without_alphabet_diacritic() + trigger
                    # Invalid diacritic: add the trigger as-is
                    case _:
                        rules[key] = key
            if pre in ["d", "đ"]:
                rules["d" + rhyme + "9"] = "đ" + rhyme
                rules["đ" + rhyme + "9"] = "d" + rhyme + "9"
    rules = dict(sorted(rules.items()))
    with open("generated-im-vni.json", "w") as f:
        json.dump(rules, f, ensure_ascii=False, indent=2)
    return rules


def main():
    prelim_table = get_preliminary_table()
    hmap = get_words_and_rhymes()
    rhyme_objs = [Rhyme(rhyme) for rhyme in sorted(set(hmap["rhymes"] + hmap["combinative_vowels"]))]
    # Make public properties dict keys
    rhyme_table = {ro._text: {k: v for k, v in ro.__dict__.items() if not k.startswith("_")} for ro in rhyme_objs}
    with open("generated-rhyme-table.json", "w") as f:
        json.dump(rhyme_table, f, ensure_ascii=False, indent=2)
    make_im_vni(rhyme_table)


if __name__ == "__main__":
    main()
