Rules for the Vietnamese input methods [VNI](https://en.wikipedia.org/wiki/VNI) (and additionally Telex for comparison purpose).

Producing rules was done by parsing words from the [Hunspell-vi](https://github.com/1ec5/hunspell-vi) dictionary and written to `generated-im-vni.json`, `generated-im-telex.json`.

# Florisboard extension

For Android [Florisboard](https://github.com/florisboard/florisboard) users, import `floris-im-vni-extension.flex` to type with VNI.

# Emacs extension

`package-vc-install` this repo, then bind `im-viet-mode` to some key.

Hopefully this will be incorporated to improve Emacs's input methods system for Vietnamese.

# How to generate rules

With [uv](https://github.com/astral-sh/uv/) installed:

```bash
uv venv
source .venv/bin/activate
python3 main.py # generated-im-{vni,telex}.json
python3 misc.py # floris-im-vni-extension.flex
```

# References:
- https://en.wikipedia.org/wiki/Vietnamese_language_and_computers#Unicode_code_points
- https://github.com/1ec5/hunspell-vi/blob/main/dictionaries/vi-DauMoi.dic
