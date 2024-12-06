import xcffib
import xcffib.xkb


def test_query_rules_names(xproto_test):
    root = xproto_test.default_screen.root

    string = xproto_test.intern("STRING")
    xkb_rules_names = xproto_test.intern("_XKB_RULES_NAMES")

    # should be enough for anybody...
    prop = xproto_test.xproto.GetProperty(
        False, root, xkb_rules_names, string, 0, 64 * 1024
    ).reply()

    strings = prop.value.to_nullsep_string()
    # xephyr's defaults
    assert strings[0] == "evdev"
    assert strings[1] == "pc105"
    assert strings[2] == "us"


def test_xkb_GetMap(xproto_test):
    xkb = xproto_test.conn(xcffib.xkb.key)

    reply = xkb.UseExtension(xcffib.xkb.MAJOR_VERSION, xcffib.xkb.MINOR_VERSION).reply()
    assert reply.supported

    core_kbd_id = xkb.GetDeviceInfo(xcffib.xkb.ID.UseCoreKbd, 0, 0, 0, 0, 0, 0).reply().deviceID

    components = (xcffib.xkb.MapPart.KeyTypes |
                  xcffib.xkb.MapPart.KeySyms |
                  xcffib.xkb.MapPart.ModifierMap |
                  xcffib.xkb.MapPart.ExplicitComponents |
                  xcffib.xkb.MapPart.KeyActions |
                  xcffib.xkb.MapPart.VirtualMods |
                  xcffib.xkb.MapPart.VirtualModMap)
    m = xkb.GetMap(
            core_kbd_id,
            components,
            0, 0, 0, 0,
            0, 0, 0, 0,
            0, 0, 0, 0,
            0, 0, 0, 0,
        ).reply()
    for k in m.modmap_rtrn:
        print(k.keycode, k.mods)
    for k in m.vmodmap_rtrn:
        print(k.keycode, k.vmods)
