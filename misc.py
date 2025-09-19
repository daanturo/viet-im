import itertools
import json
import os
import re
import shutil
import subprocess
import tempfile
import urllib.request
import zipfile
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


def compare_with_floris_telex():
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
    # We handle d...d -> đ...
    generated = {
        k: v
        for (k, v) in json.loads(Path(generated_file).read_text()).items()
        if not re.search(r"^[dđ][^dđ]", k)
    }
    diff_lst = dict_diff(generated, floris)
    Path("generated-rules-diff-this-telex-vs-floris-telex.json").write_text(
        json.dumps(
            {
                "floris - this": diff_lst[1],
                "this - floris": diff_lst[0],
            },
            indent=2,
            ensure_ascii=False,
        )
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
        # No upper case handling yet
        for k in keys:
            v = rules[k]
            f.write(f' ("{k}"  ["{v}"])\n')
        f.write(")" + "\n")


def write_floris_vni_extension():
    rules = main.make_im_vni()
    groups = main.group_rules_for_write(rules.keys())
    indent = " " * 8  # depend on inserting position below
    rules_str = (
        "\n".join(
            map(
                lambda g: indent + " ".join(map(lambda k: f'"{k}": "{rules[k]}",', g)),
                groups,
            )
        )
    ).removesuffix(",")
    # https://github.com/florisboard/florisboard/wiki/Creating-and-Packaging-Extensions
    # Can't https://github.com/florisboard/florisboard/wiki/How-to-publish-on-FlorisBoard-Addons#register-account for now
    # Happy to change the id to just "vni" if accepted upstream.
    extension = (
        """{
  "$": "ime.extension.keyboard",
  "meta": {
    "id": "io.github.daanturo.im-viet-rules.vni",
    "version": "0.1.0",
    "title": "Vietnamese VNI input method",
    "description": "Provide the Vietnamese (Tan Ky mode) VNI input method for Florisboard, in addition to the built-in Telex.\\n  See: https://en.wikipedia.org/wiki/VNI#Input_methods.",
    "keywords": ["composer", "input-method"],
    "maintainers": ["@daanturo"],
    "license": "mpl-2.0"
  },
  "composers": [
    {
      "$": "with-rules",
      "id": "vni-addon",
      "label": "VNI",
      "rules": {"""
        + "\n"
        + rules_str
        + """
      }
    }
  ]
}\n"""
    )
    path = "floris-im-vni-extension.json"
    Path(path).write_text(extension, encoding="utf8")
    shutil.copy(path, "extension.json")
    # No idea how to generate a .flex archive file, none of the web searching,
    # Mistral, chatGPT, Gemini knew, until I did a org-wide search and stumbled
    # at
    # https://github.com/florisboard/nlp/blob/9e8d49649d6a3d32af85873043ba2c3c0a43d151/utils/convert_dictionaries_to_extensions.py#L56
    with zipfile.ZipFile("floris-im-vni-extension.flex", "w") as f:
        f.write("extension.json")
    os.remove("extension.json")


if __name__ == "__main__":
    compare_with_floris_telex()
    # save_emacs_quail_rules()
    write_floris_vni_extension()
