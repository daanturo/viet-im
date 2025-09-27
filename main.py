import unittest

import addons
import core


class TestInputMethod(unittest.TestCase):
    def test_on_dictionary(self):
        words = core.get_words_from_dictionary()
        failed = 0
        for im in ["telex", "vni"]:
            for word in words:
                ways = core.ways_to_type_word(word, im)
                typed_composed = [(s, core.compose_diacritics(s, im)) for s in ways]
                failed_ways = [e for e in typed_composed if e[1] != word]
                failed += failed_ways != []
                with self.subTest(word=word, im=im, failed_ways=failed_ways):
                    self.assertEqual(failed_ways, [])
        print(f"Tested on {len(words)} words.")


def main():
    rhyme_table = core.get_full_rhyme_table()
    core.make_im_vni(rhyme_table)
    core.make_im_telex(rhyme_table)

    print("Testing...")
    unittest.main(exit=False)

    addons.compare_with_floris_telex()
    addons.write_floris_vni_extension()
    addons.write_emacs_package_rules_file()


if __name__ == "__main__":
    main()
