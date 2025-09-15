import json
import os
import re
import tempfile
import urllib.request
from pathlib import Path

import main


def dict_diff(dict1, dict2):
    diff1 = dict()
    diff2 = dict()
    for key in set(list(dict1.keys()) + list(dict2.keys())):
        val1 = dict1.get(key)
        val2 = dict2.get(key)
        if val1 != val2:
            if key in dict1:
                diff1[key] = val1
            if key in dict2:
                diff2[key] = val2
    return [dict(sorted(diff1.items())), dict(sorted(diff2.items()))]


def compare_with_florish_telex():
    url = "https://raw.githubusercontent.com/florisboard/florisboard/refs/heads/main/app/src/main/assets/ime/keyboard/org.florisboard.composers/extension.json"
    floris_file = os.path.join(tempfile.gettempdir(), "floris-extension.json")
    generated_file = os.path.abspath("generated-im-telex.json")
    if not os.path.exists(floris_file):
        urllib.request.urlretrieve(url, filename=floris_file)
    # print(f"Comparing: '{generated_file}', '{floris_file}'")
    floris = [
        composer["rules"]
        for composer in json.loads(Path(floris_file).read_text())["composers"]
        if composer.get("id") == "telex"
    ][0]
    generated = {
        k: v
        for (k, v) in json.loads(Path(generated_file).read_text()).items()
        if not re.search(r"^[dđ]", k)
    }
    Path("temp-rules-diff.json").write_text(
        json.dumps(dict_diff(generated, floris), indent=2, ensure_ascii=False)
    )


def save_emacs_quail_rules():
    """Problem with quail: after finishing a diacritic, typing more won't
    modify the word, unless exhaustively listing all variants.
    """
    rules = main.make_im_vni(main.make_full_rhyme_table())
    # Prioritize longer?
    keys = sorted(rules.keys(), key=lambda s: (-len(s), s))
    with open("temp-vn-vni-x.el", "w") as f:
        f.write(";; -*- lexical-binding: t -*-" + "\n\n")
        f.write("(require 'quail)\n\n")
        f.write("""
(quail-define-package
 "vietnamese-vni-x"              ; NAME
 "Vietnamese"                    ; LANGUAGE
 "VVX"                           ; TITLE
 t                               ; GUIDANCE
 "VNI input method, with Tan Ky mode on, allow user to type diacritical marks
 anywhere within a word and the marks will appear at their proper locations.
"                                ; DOCSTRING
 nil                             ; TRANSLATION-KEYS
 t                               ; FORGET-LAST-SELECTION
 nil                             ; DETERMINISTIC
 nil                             ; KBD-TRANSLATE
 nil                             ; SHOW-LAYOUT
 nil                             ; CREATE-DECODE-MAP
 nil                             ; MAXIMUM-SHORTEST
 nil                             ; OVERLAY-PLIST
 nil                             ; UPDATE-TRANSLATION-FUNCTION
 nil                             ; CONVERSION-KEYS
 t)
\n""")
        f.write("(quail-define-rules" + "\n")
        for k in keys:
            v = rules[k]
            f.write(f' ("{k}"  ["{v}"])\n')
        f.write(")" + "\n")


if __name__ == "__main__":
    compare_with_florish_telex()
    save_emacs_quail_rules()
