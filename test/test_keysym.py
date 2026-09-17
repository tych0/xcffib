import xcffib.keysymdef
import xcffib.xf86keysym


def test_keysymdef_values():
    assert xcffib.keysymdef.XK_A == 0x0041
    assert xcffib.keysymdef.XK_Escape == 0xFF1B
    assert xcffib.keysymdef.XK_Arabic_0 == 0x1000660


def test_xf86keysym_values():
    assert xcffib.xf86keysym.XF86XK_AudioMute == 0x1008FF12
    assert xcffib.xf86keysym.XF86XK_AudioPlay == 0x1008FF14
    assert xcffib.xf86keysym.XF86XK_Assistant == 0x10081000 + 0x247
