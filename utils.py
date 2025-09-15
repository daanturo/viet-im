import json
import os
import re
import tempfile
import urllib.request
from pathlib import Path


def compare_with_florish_telex():
    url = "https://raw.githubusercontent.com/florisboard/florisboard/refs/heads/main/app/src/main/assets/ime/keyboard/org.florisboard.composers/extension.json"
    floris_file = os.path.join(tempfile.gettempdir(), "floris-extension.json")
    generated_file = os.path.abspath("generated-im-telex.json")
    if not os.path.exists(floris_file):
        urllib.request.urlretrieve(url, filename=floris_file)
    print(f"Comparing: '{generated_file}', '{floris_file}'")
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
