import argparse
import json
import os
import re
import sys
import unicodedata
import urllib
from copy import deepcopy
from functools import reduce
from typing import Self

import polars as pl
from unidecode import unidecode

## * Helpers


def polars_value_position(df, value):
    for col in df.columns:
        row_idx = df[col].index_of(value)
        if row_idx is not None:
            return (row_idx, col)
    return None


## * Constants

# huyền hỏi ngã sắc nặng
TONE_LIST = ["grave", "hook", "perispomeni", "acute", "dot_below"]

VOWEL_DF = pl.DataFrame(
    {
        "unmarked   ": ["a", "ă", "â", "e", "ê", "i", "o", "ô", "ơ", "u", "ư", "y"],
        "grave      ": ["à", "ằ", "ầ", "è", "ề", "ì", "ò", "ồ", "ờ", "ù", "ừ", "ỳ"],
        "hook       ": ["ả", "ẳ", "ẩ", "ẻ", "ể", "ỉ", "ỏ", "ổ", "ở", "ủ", "ử", "ỷ"],
        "perispomeni": ["ã", "ẵ", "ẫ", "ẽ", "ễ", "ĩ", "õ", "ỗ", "ỡ", "ũ", "ữ", "ỹ"],
        "acute      ": ["á", "ắ", "ấ", "é", "ế", "í", "ó", "ố", "ớ", "ú", "ứ", "ý"],
        "dot_below  ": ["ạ", "ặ", "ậ", "ẹ", "ệ", "ị", "ọ", "ộ", "ợ", "ụ", "ự", "ỵ"],
        "ascii      ": ["a", "a", "a", "e", "e", "i", "o", "o", "o", "u", "u", "y"],
    }
)
VOWEL_DF = VOWEL_DF.rename(lambda s: s.strip())  # For pretty alignment above, then trim
VOWEL_DF = VOWEL_DF.with_columns(
    (
        pl.col("unmarked").map_elements(
            lambda c: {
                "ă": "breve",
                "â": "circumflex",
                "ê": "circumflex",
                "ô": "circumflex",
                "ơ": "horn",
                "ư": "horn",
            }.get(c, None)
        )
    ).alias("alphabet_diacritic")
)
# Refer to the non-diacritic's index
VOWEL_DF = VOWEL_DF.join(
    VOWEL_DF.with_row_index()
    .group_by("ascii", maintain_order=True)
    .agg(pl.col("index").min().alias("ascii_index")),
    on="ascii",
    maintain_order="left",
)
VOWEL_DF = VOWEL_DF.with_row_index()

NO_TONAL_VOWEL_LETTERS = "".join(VOWEL_DF["unmarked"])
TONE_GRAVE_LETTERS = "".join(VOWEL_DF["grave"])
TONE_HOOK_LETTERS = "".join(VOWEL_DF["hook"])
TONE_PERISPOMENI_LETTERS = "".join(VOWEL_DF["perispomeni"])
TONE_ACUTE_LETTERS = "".join(VOWEL_DF["acute"])
TONE_DOT_BELOW_LETTERS = "".join(VOWEL_DF["dot_below"])
TONAL_VOWEL_LETTERS = (
    TONE_GRAVE_LETTERS
    + TONE_HOOK_LETTERS
    + TONE_PERISPOMENI_LETTERS
    + TONE_ACUTE_LETTERS
    + TONE_DOT_BELOW_LETTERS
)
VOWEL_LETTERS = NO_TONAL_VOWEL_LETTERS + TONAL_VOWEL_LETTERS
CONSONANT_LETTERS = "bcdfgjklmnpqstvxzhrw" + "đ"
COMBINATIVE_CONSONANTS_WITH_VOWEL_LETTERS = ["gi", "qu"]

preliminary_rules_file = "preliminary-rules.json"
try:
    with open(preliminary_rules_file) as f:
        preliminary_rhyme_table = json.load(f)
except Exception:
    preliminary_rhyme_table = dict()


def remove_letter_alphabet_diacritic(char: str):
    "Keep tone."
    rc = polars_value_position(VOWEL_DF, char)
    if not rc:
        return char
    row, col = rc
    new_row = VOWEL_DF[row, "ascii_index"]
    return VOWEL_DF[new_row, col]


def remove_letter_tone(char: str):
    "Keep alphabet diacritic."
    rc = polars_value_position(VOWEL_DF, char)
    if not rc:
        return char
    return VOWEL_DF[rc[0], "unmarked"]


### * (Combinative) Vowels


class Rhyme:
    """The vowel part is either elemental or combinative.  In a vowel, for each
    of alphabet diacritic and tone, there is only at most one class."""

    _text: str
    _alphabet_diacritic = None
    _tone = None
    _letter_df = VOWEL_DF
    _vowel_part = ""
    _suffix_consonant_part = ""

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
        self._text = text

        found = False
        for i in range(len(text)):
            char = text[i]
            rc = polars_value_position(self._letter_df, char)
            if rc and (rc[1] in TONE_LIST):
                self._tone = rc[1]
                self.tone_position = i
                found = True
                break
        if not found:
            self._tone = "unmarked"

        for char in text:
            rc = polars_value_position(self._letter_df, char)
            if rc:
                self._alphabet_diacritic = self._letter_df[rc[0], "alphabet_diacritic"]
                if self._alphabet_diacritic:
                    break

        m = re.match(
            rf"^([{VOWEL_LETTERS}]+)([{CONSONANT_LETTERS}]+)?$",
            text,
        )
        assert m.group(1)
        self._vowel_part = m.group(1)
        self._suffix_consonant_part = m.group(2) or ""

    def _to_object_maybe(self, txt: str, to_object: bool) -> str | Self:
        if to_object:
            return Rhyme(txt)
        return txt

    def without_alphabet_diacritic(self, to_object=False) -> str | Self:
        txt = "".join(remove_letter_alphabet_diacritic(c) for c in self._text)
        return self._to_object_maybe(txt, to_object)

    def without_tone(self, to_object=False) -> str | Self:
        txt = "".join(remove_letter_tone(c) for c in self._text)
        return self._to_object_maybe(txt, to_object)

    def base(self, to_object=False) -> str | Self:
        txt = unidecode(self._text)
        self.ascii = txt
        return self._to_object_maybe(txt, to_object)

    def with_alphabet_diacritic(
        self, apb_diacriatic: str, rhyme_table: dict, to_object=False
    ) -> str | Self | None:
        ascii_vowels = unidecode(self._vowel_part)
        possible = self._letter_df.filter(
            pl.col("alphabet_diacritic").is_in([apb_diacriatic]),
            pl.col("ascii").is_in(list(ascii_vowels)),
        )
        if len(possible) == 0:
            return None
        new_vowels = []
        for i in range(len(ascii_vowels)):
            char = ascii_vowels[i]
            # a duplicate ASCII vowel in a rhyme don't get have diacritics ("ưu", "ươu")
            if char in ascii_vowels[:i]:
                new_vowels += [char]
            else:
                new_vowel_search = possible.filter(pl.col("ascii").is_in([char]))
                if len(new_vowel_search) > 0:
                    new_vowels += [new_vowel_search[0, self._tone]]
                else:
                    new_vowels += [self._vowel_part[i]]
        retval = "".join(new_vowels) + self._suffix_consonant_part
        return retval

    def with_tone(self, tone: str, to_object=False) -> str | Self:
        return tone.code_point


## * Process


def get_words_from_dictionary():
    dict_filename = "vi-DauMoi.dic"
    if not os.path.exists(dict_filename):
        urllib.request.urlretrieve(
            "https://github.com/1ec5/hunspell-vi/raw/refs/heads/main/dictionaries/vi-DauMoi.dic",
            filename=dict_filename,
        )
    with open(dict_filename) as f:
        words = f.read().splitlines()
    # Ignore words with upper case letter and numbers
    words = [
        word
        for word in words
        if (not (any(map(lambda c: c.isupper(), word)) or re.search(r"[0-9]", word)))
    ]
    return words


def get_vowels_and_consonants(words):
    van_list = words
    # Strip special "phụ âm đầu"s that include a vowel
    van_list = [
        reduce(
            lambda w, p: w.removeprefix(p),
            COMBINATIVE_CONSONANTS_WITH_VOWEL_LETTERS,
            word,
        )
        for word in van_list
    ]

    prefix_consonants = set(COMBINATIVE_CONSONANTS_WITH_VOWEL_LETTERS)
    rhymes = set()
    tonal_combin_vowels = set()
    suffix_consonants = set()

    for van in van_list:
        m = re.match(
            rf"^([{CONSONANT_LETTERS}]+)?(([{VOWEL_LETTERS}]+)([{CONSONANT_LETTERS}]+)?)$",
            van,
        )
        if m:
            if m.group(1):
                prefix_consonants.add(m.group(1))
            rhymes.add(m.group(2))
            if m.group(3):
                tonal_combin_vowels.add(m.group(3))
            if m.group(4):
                suffix_consonants.add(m.group(4))

    return (
        sorted(prefix_consonants),
        sorted(rhymes),
        sorted(tonal_combin_vowels),
        sorted(suffix_consonants),
    )


def main():
    words = get_words_from_dictionary()
    prefix_consonants, rhymes, tonal_combin_vowels, suffix_consonant = (
        get_vowels_and_consonants(words)
    )

    no_tonal_combin_vowels = set(
        "".join(map(remove_letter_tone, cvowel)) for cvowel in tonal_combin_vowels
    )

    assert (dict() == preliminary_rhyme_table) or (
        list(preliminary_rhyme_table.keys()) == no_tonal_combin_vowels
    )

    rhyme_objs = [Rhyme(s) for s in tonal_combin_vowels]
    im_rhyme_table = deepcopy(preliminary_rhyme_table)
    for rhyme in rhyme_objs:
        if im_rhyme_table.get(rhyme.without_tone()) is None:
            im_rhyme_table[rhyme.without_tone()] = dict()
        # Mark tone position of non tonal rhymes using their tonal variants
        im_rhyme_table[rhyme.without_tone()]["tone_position"] = rhyme.tone_position

    if not os.path.exists(preliminary_rules_file):
        with open(preliminary_rules_file, "w", encoding="utf8") as f:
            json.dump(im_rhyme_table, f, ensure_ascii=False, indent=2)


if __name__ == "__main__":
    main()
