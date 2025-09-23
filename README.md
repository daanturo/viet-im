Rules for the Vietnamese input methods [VNI](https://en.wikipedia.org/wiki/VNI) (and additionally Telex for comparison purpose).

Producing rules was done by parsing words from the [Hunspell-vi](https://github.com/1ec5/hunspell-vi) dictionary and written to `generated-im-vni.json`, `generated-im-telex.json`.

# Florisboard extension

For Android [Florisboard](https://github.com/florisboard/florisboard) users, import `floris-im-vni-extension.flex` to type with VNI.

# Emacs extension

Install:
```elisp
(package-vc-install '(im-viet :url "https://gitlab.com/daanturo/im-viet"))
```

Then call `set-input-method`/`toggle-input-method` (`toggle-input-method` need prefix argument if it was already called in this Emacs session) and select `vietnamese-vni-x` or `vietnamese-telex-x`.

Hopefully this will be incorporated to improve Emacs's input methods system for Vietnamese.

The package current supports VNI, Telex is secondary, VIQR and other input methods aren't planned at the moment.

Related issue: [https://lists.gnu.org/archive/html/help-gnu-emacs/2025-09/msg00205.html](https://yhetil.org/emacs/86a52lej8i.fsf@gnu.org/)

# How to generate rules

With [uv](https://github.com/astral-sh/uv/) installed:

```bash
uv venv
source .venv/bin/activate
python3 main.py # generated-im-{vni,telex}.json
python3 misc.py # floris-im-vni-extension.flex
```

Processing steps on a high-level:
- From words in dictionary, extract rhymes
- Generate `generated-preliminary-table.json`: core information - rhymes with their tone positions
- Generate `generated-rhyme-table.json`: describes how each rhyme response to each diacritic change
- Generate `generated-im-telex.json` & `generated-im-vni.json`: rules for those input methods

# References:
- https://en.wikipedia.org/wiki/Vietnamese_language_and_computers#Unicode_code_points
- https://github.com/1ec5/hunspell-vi/blob/main/dictionaries/vi-DauMoi.dic
