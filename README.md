Goal:
Make a `rhyme-rules.json` table, where each key is a "nguyên âm/vần" word part, that describe how that word part reacts to changes.

```json
{
  // ...
  "ướu": {
    "ascii": "uou",

    // không dấu
    "no_alphabet_diacritic": "uóu",
    // "ă"
    "breve": "ướu", // unresponsive
    // "âêô"
    "circumflex": "ướu", // unresponsive
    // "ơư"
    "horn": "ướu", // same->remove, vni: "uóu7", telex: "uóuw"

    "tone_position": 1, // 0-based indexing

    // không thanh điệu (tone)
    "no_tone": "ươu",
    // huyền
    "grave": "ườu",
    // hỏi
    "hook": "ưởu",
    // ngã
    "perispomeni": "ưỡu",
    // sắc
    "acute": "ướu", // same->remove, vni: "ươu1", telex: "ươus"
    // nặng
    "dot_below": "ượu",
  }
  // ...
}
```

`preliminary-rules.json` is a starting point that only includes non-tonals vowels and essential information such as tone_position.

# References:
- https://en.wikipedia.org/wiki/Vietnamese_language_and_computers#Unicode_code_points
- https://github.com/1ec5/hunspell-vi/blob/main/dictionaries/vi-DauMoi.dic
