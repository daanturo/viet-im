Goal:
Make a `lookup-nguyen-am-ghep.jsonc` table, where each key is a "nguyên âm", that describe how that nguyên âm react to changes.

```json
{
  // ...
  "ướu": {
    "ascii": "uou",

    // không dấu
    "no_alphabet_diacritic": "uóu",
    // "ă"
    "breve": null, // unresponsive
    // "âêô"
    "circumflex": null, // unresponsive
    // "ơư"
    "horn": false, // same->remove, vni: "uóu7", telex: "uóuw"

    "tone_position": 1,

    // không thanh điệu (tone)
    "no_tone": "ươu",
    // huyền
    "grave": "ườu",
    // hỏi
    "hook": "ưởu",
    // ngã
    "perispomeni": "ưỡu",
    // sắc
    "acute": false, // same->remove, vni: "ươu1", telex: "ươus"
    // nặng
    "dot_below": "ượu",
  }
  // ...
}
```
