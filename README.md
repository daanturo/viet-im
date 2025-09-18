Rules for the Vietnamese input methods [VNI](https://en.wikipedia.org/wiki/VNI) (and additionally Telex for comparison purpose).

Producing rules was done by parsing words from the [Hunspell-vi](https://github.com/1ec5/hunspell-vi) dictionary and written to `generated-im-vni.json`, `generated-im-telex.json`.


For Android [Florisboard](https://github.com/florisboard/florisboard) users, import `floris-im-vni-extension.flex` to type with VNI.

# How to run

With [uv](https://github.com/astral-sh/uv/) installed:

```bash
uv venv
source .venv/bin/activate
python3 main.py
python3 misc.py
```

# References:
- https://en.wikipedia.org/wiki/Vietnamese_language_and_computers#Unicode_code_points
- https://github.com/1ec5/hunspell-vi/blob/main/dictionaries/vi-DauMoi.dic
