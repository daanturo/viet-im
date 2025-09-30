Rules for the Vietnamese input methods [VNI](https://en.wikipedia.org/wiki/VNI) (and additionally Telex for comparison purpose).

Producing rules was done by parsing words from the [Hunspell-vi](https://github.com/1ec5/hunspell-vi) dictionary and written to `generated-im-vni.json`, `generated-im-telex.json`.

# Florisboard extension

For Android [Florisboard](https://github.com/florisboard/florisboard) users, import `floris-im-vni-extension.flex` to type with VNI.

# Emacs extension

Install:
```elisp
(package-vc-install '(viet-im :url "https://gitlab.com/daanturo/viet-im"))
```

Then call `set-input-method`/`toggle-input-method` (`toggle-input-method` need prefix argument if it was already called in this Emacs session) and select `vietnamese-vni-x` or `vietnamese-telex-x`.

The package currently supports VNI, Telex (secondary), VIQR and other input methods aren't planned at the moment.  VIQR is conceptually similar to VNI and Telex, the only (IIRC) difference among the 3 IMs is at triggering characters so adding support for it shouldn't be hard if needed.

Related issue: [https://lists.gnu.org/archive/html/help-gnu-emacs/2025-09/msg00205.html](https://yhetil.org/emacs/86a52lej8i.fsf@gnu.org/).  [Tan Ky mode](https://en.wikipedia.org/wiki/VNI#VNI_Tan_Ky) is supported for both VNI and Telex, allows typing diacritics anywhere after the base letters.

# How to generate rules

With [uv](https://github.com/astral-sh/uv/) installed:

```bash
uv venv
source .venv/bin/activate
python3 main.py
```

Processing steps on a high-level:
- From words in dictionary, extract rhymes
- Generate `generated-preliminary-table.json`: core information - rhymes with their tone positions
- Generate `generated-rhyme-table.json`: describes how each rhyme response to each diacritic change
- Generate `generated-im-telex.json` & `generated-im-vni.json`: rules for those input methods

# References:
- https://en.wikipedia.org/wiki/Vietnamese_language_and_computers#Unicode_code_points
- https://github.com/1ec5/hunspell-vi/blob/main/dictionaries/vi-DauMoi.dic
