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
    "breve": {
        "code_point": 0x0306,
        "letters": "ă",
    },
    "circumflex": {
        "code_point": 0x0302,
        "letters": "âêô",
    },
    "horn": {
        "code_point": 0x031B,
        "letters": "ơư",
    },
}
TONE_TABLE = {
    "unmarked": {
        "code_point": None,
        "name": "ngang",
    },
    "grave": {
        "code_point": 0x0300,
        "name": "huyền",
    },
    "hook": {
        "code_point": 0x0309,
        "name": "hỏi",
    },
    "perispomeni": {
        "code_point": 0x0303,
        "name": "ngã",
    },
    "acute": {
        "code_point": 0x0301,
        "name": "sắc",
    },
    "dot_below": {
        "code_point": 0x0323,
        "name": "nặng",
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


def make_preliminary_rhyme_table(combin_vowels):
    with open("./manual-tone-positions.json", encoding="utf-8") as f:
        manual_tone_positions = json5.load(f)
    rhyme_table = dict()
    for rhyme in combin_vowels:
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
    return rhyme_table


preliminary_table_file = "generated-preliminary-table.json"
_preliminary_rhyme_table = None
# try:
#     with open(preliminary_table_file) as f:
#         _preliminary_rhyme_table = json5.load(f)
# except Exception: pass


def _init_preliminary_table(combin_vowels):
    rhyme_table = make_preliminary_rhyme_table(combin_vowels)
    rhyme_table_no_tone = {k: v for (k, v) in rhyme_table.items() if not get_string_tone(k)}
    if True:
        with open(preliminary_table_file, "w", encoding="utf8") as f:
            json.dump(rhyme_table_no_tone, f, ensure_ascii=False, indent=2)

    return rhyme_table_no_tone


_preliminary_rhyme_table_type = "rhymes"  # "combinative_vowels"


def get_preliminary_table():
    global _preliminary_rhyme_table

    if _preliminary_rhyme_table is None:
        hmap = get_words_and_rhymes()
        _preliminary_rhyme_table = _init_preliminary_table(hmap[_preliminary_rhyme_table_type])
    return _preliminary_rhyme_table


class Rhyme:
    """The vowel part is either elemental or combinative.  In a vowel, for each
    of alphabet diacritic and tone, there is only at most one class."""

    _text: str
    _alphabet_diacritic = None
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

    no_tone = None
    grave = None
    hook = None
    perispomeni = None
    acute = None
    dot_below = None

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
            self._alphabet_diacritic = None
        else:
            self._alphabet_diacritic = found["alphabet_diacritic"]

        self.ascii = remove_string_all_diacritics(text)
        self.no_alphabet_diacritic = remove_string_alphabet_diacritics(text)
        self.no_tone = remove_string_tone_diacritics(text)

        self.breve = self.with_alphabet_diacritic("breve")
        self.circumflex = self.with_alphabet_diacritic("circumflex")
        self.horn = self.with_alphabet_diacritic("horn")

        self.grave = self.with_tone("grave")
        self.hook = self.with_tone("hook")
        self.perispomeni = self.with_tone("perispomeni")
        self.acute = self.with_tone("acute")
        self.dot_below = self.with_tone("dot_below")

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


def main():
    prelim_table = get_preliminary_table()
    hmap = get_words_and_rhymes()
    rhyme_objs = [Rhyme(rhyme) for rhyme in hmap["rhymes"]]
    rhyme_table = {ro._text: {k: v for k, v in ro.__dict__.items() if not k.startswith("_")} for ro in rhyme_objs}
    with open("generated-rhyme-table.json", "w") as f:
        json.dump(rhyme_table, f, ensure_ascii=False, indent=2)


if __name__ == "__main__":
    main()
