# Copyright 2014 Tycho Andersen
# Copyright 2014 Sean Vig
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import xcffib
import xcffib.xproto
import xcffib.xinput
import xcffib.randr
import os
import struct
import sys
from xcffib.ffi import ffi
from xcffib.xproto import EventMask

from .conftest import XcffibTest


class TestPythonCode:
    def test_struct_pack_uses_List(self):
        # suppose we have a list of ints...
        ints = struct.pack("=IIII", *range(4))

        # Unpacker wants a cffi.cdata
        cffi_ints = xcffib.ffi.new("char[]", ints)

        data = xcffib.List(xcffib.CffiUnpacker(cffi_ints), "I", count=4)
        ints2 = struct.pack("=IIII", *data)

        # after packing and unpacking, we should still have those ints
        assert ints == ints2

    def test_union_pack(self):
        data = struct.pack("=" + ("b" * 20), *range(20))
        cffi_data = xcffib.ffi.new("char[]", data)

        cm = xcffib.xproto.ClientMessageData(xcffib.CffiUnpacker(cffi_data))

        for actual, expected in zip(range(20), cm.data8):
            assert actual == expected, actual

        if sys.byteorder == "little":
            assert cm.data32[0] == 0x03020100
            assert cm.data32[1] == 0x07060504
            assert cm.data32[2] == 0x0B0A0908
        elif sys.byteorder == "big":
            assert cm.data32[0] == 0x00010203
            assert cm.data32[1] == 0x04050607
            assert cm.data32[2] == 0x08090A0B
        else:
            raise Exception("unknown byte order?")

    def test_offset_map(self):
        om = xcffib.OffsetMap({0: "Event0,0"})
        om.add(1, 0, {0: "Event1,0", 1: "Event1,1"})

        assert om[0] == "Event0,0"
        assert om[1] == "Event1,0"
        assert om[2] == "Event1,1"

        om.add(10, 20, {5: "ExtensionEvent20,5", 6: "ExtensionEvent20,6"})
        assert om.get_extension_item(20, 5) == "ExtensionEvent20,5"
        assert om.get_extension_item(20, 6) == "ExtensionEvent20,6"

    def test_create_ClientMessageEvent(self, xcffib_test):
        wm_protocols = xcffib_test.intern("WM_PROTOCOLS")
        wm_delete_window = xcffib_test.intern("WM_DELETE_WINDOW")

        # should be exactly 20 bytes
        data = [
            wm_delete_window,
            xcffib.xproto.Time.CurrentTime,
            0,
            0,
            0,
        ]

        union = xcffib.xproto.ClientMessageData.synthetic(data, "I" * 5)
        assert list(union.data32) == data

        wid = xcffib_test.conn.generate_id()
        xcffib_test.create_window(wid=wid)

        wm_protocols = xcffib_test.intern("WM_PROTOCOLS")
        wm_delete_window = xcffib_test.intern("WM_DELETE_WINDOW")

        e = xcffib.xproto.ClientMessageEvent.synthetic(
            format=32, window=wid, type=wm_protocols, data=union
        )

        xcffib_test.xproto.SendEvent(False, wid, EventMask.NoEvent, e.pack())
        xcffib_test.conn.flush()

        e = xcffib_test.conn.wait_for_event()
        assert isinstance(e, xcffib.xproto.ClientMessageEvent)
        assert e.window == wid
        assert list(e.data.data32) == data

    def test_pack_from_event(self, xcffib_test):
        wm_protocols = xcffib_test.intern("WM_PROTOCOLS")
        wm_delete_window = xcffib_test.intern("WM_DELETE_WINDOW")
        wid = xcffib_test.conn.generate_id()

        # should be exactly 20 bytes
        data = [
            wm_delete_window,
            xcffib.xproto.Time.CurrentTime,
            0,
            0,
            0,
        ]

        union = xcffib.xproto.ClientMessageData.synthetic(data, "I" * 5)
        e = xcffib.xproto.ClientMessageEvent.synthetic(
            format=32, window=wid, type=wm_protocols, data=union
        )

        _ = xcffib.xproto.ClientMessageEvent(e)

    def test_get_image(self, xcffib_test):
        # adapted from: https://gist.github.com/liftoff/4741790
        setup = xcffib_test.conn.get_setup()
        screen = setup.roots[0]
        width = screen.width_in_pixels
        height = screen.height_in_pixels
        root = screen.root

        # GetImage requires an output format as the first arg.  We want ZPixmap:
        output_format = xcffib.xproto.ImageFormat.ZPixmap
        plane_mask = 2**32 - 1  # No idea what this is but it works!
        reply = xcffib_test.conn.core.GetImage(
            output_format, root, 0, 0, width, height, plane_mask
        ).reply()
        reply.data.buf()

    def test_ge_generic_event_hoist(self, xcffib_test):
        """Tests the ability to hoist events to the correct extension event."""

        # Create a bytearray representing a BarrierHitEvent from XInput
        B_HIT_EVENT = struct.pack(
            "=BBHIHHIIIIIIIIH2xiiQQ",
            35,  # response_type
            131,  # extension
            1,  # sequence
            9,  # length
            25,  # event_type
            2,  # device_id
            0,  # time
            1,  # event_id
            0,  # root
            0,  # event
            0,  # barrier
            1,  # full_sequence
            0,  # d_time
            0,  # flags
            11,  # source_id
            100 << 16,  # root_x
            200 << 16,  # root_y
            0,  # dx
            0,  # dy
        )

        # Create cdata from the bytearray and cast it to a generic reply
        cdata = ffi.new("char x[72]", B_HIT_EVENT)
        generic_reply = ffi.cast("xcb_generic_reply_t *", cdata)

        # Pass the reply to our hoist_event method
        event = xcffib_test.conn.hoist_event(generic_reply)

        assert isinstance(event, xcffib.xinput.BarrierHitEvent)
        assert event.root_x >> 16 == 100
        assert event.root_y >> 16 == 200

    def test_List_to_string(self, xcffib_test):
        xrandr = xcffib_test.conn(xcffib.randr.key)
        try:
            setup = xcffib_test.conn.get_setup()
            for screen in setup.roots:
                scrn_rsrcs = xrandr.GetScreenResources(screen.root).reply()
                for output in scrn_rsrcs.outputs:
                    info = xrandr.GetOutputInfo(output, xcffib.XCB_CURRENT_TIME).reply()
                    print(info.name.to_string())
                    assert info.name.to_string() == "screen"
        finally:
            xcffib_test.conn.disconnect()

    def test_replies_are_hashable(self, xcffib_test):
        # I have written code that relied on replies being hash()-able, and
        # we have broken that. So let's generate a reply and hash() it and make
        # sure that works.
        setup = xcffib_test.conn.get_setup()
        hash(setup)

    def test_protobj_repr_is_reasonable(self, xcffib_test):
        # make sure that repr() does something reasonable
        setup = xcffib_test.conn.get_setup()
        assert "FORMAT(bits_per_pixel=1, bufsize=8, depth=1, fixed_size=8, scanline_pad=32, xge=False)" == repr(setup.pixmap_formats[0])

    def test_list_repr_of_chars_is_reasonable(self, xcffib_test):
        setup = xcffib_test.conn.get_setup()
        assert "\"The X.Org Foundation\"" == repr(setup.vendor)


class TestXcffibTestGenerator:
    def test_XcffibTest_generator(self):
        try:
            old_display = os.environ["DISPLAY"]
        except KeyError:
            old_display = ""
        # use some non-default width/height
        with XcffibTest(width=1001, height=502) as test:
            assert os.environ["DISPLAY"] != old_display
            setup = test.conn.get_setup()
            screen = setup.roots[0]
            width = screen.width_in_pixels
            height = screen.height_in_pixels
            assert width == 1001
            assert height == 502
