import xcffib
import xcffib.xproto

def test_query_rules_names(xproto_test):
    setup = xproto_test.conn.get_setup()

    root = xproto_test.default_screen.root

    string = xproto_test.intern("STRING")
    xkb_rules_names = xproto_test.intern("_XKB_RULES_NAMES")

    # should be enough for anybody...
    prop = xproto_test.xproto.GetProperty(False, root, xkb_rules_names, string, 0, 64 * 1024).reply()

    strings = prop.value.to_nullsep_string()
    # xephyr's defaults
    assert strings[0] == "evdev"
    assert strings[1] == "pc105"
    assert strings[2] == "us"
