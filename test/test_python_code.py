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
import struct
import sys
from xcffib.xproto import EventMask

from .testing import XcffibTest

class TestPythonCode(XcffibTest):

    def test_struct_pack_uses_List(self):
        # suppose we have a list of ints...
        ints = struct.pack("=IIII", *range(4))

        # Unpacker wants a cffi.cdata
        cffi_ints = xcffib.ffi.new('char[]', ints)

        l = xcffib.List(xcffib.CffiUnpacker(cffi_ints), "I", count=4)
        ints2 = struct.pack("=IIII", *l)

        # after packing and unpacking, we should still have those ints
        assert ints == ints2

    def test_union_pack(self):
        data = struct.pack("=" + ("b" * 20), *range(20))
        cffi_data = xcffib.ffi.new('char[]', data)

        cm = xcffib.xproto.ClientMessageData(xcffib.CffiUnpacker(cffi_data))

        for actual, expected in zip(range(20), cm.data8):
            assert actual == expected, actual

        if sys.byteorder == "little":
            assert cm.data32[0] == 0x03020100
            assert cm.data32[1] == 0x07060504
            assert cm.data32[2] == 0x0b0a0908
        elif sys.byteorder == "big":
            assert cm.data32[0] == 0x00010203
            assert cm.data32[1] == 0x04050607
            assert cm.data32[2] == 0x08090a0b
        else:
            raise Exception("unknown byte order?")

    def test_offset_map(self):
        om = xcffib.OffsetMap({0: "Event0,0"})
        om.add(1, {0: "Event1,0", 1: "Event1,1"})

        assert om[0] == "Event0,0"
        assert om[1] == "Event1,0"
        assert om[2] == "Event1,1"

    def test_create_ClientMessageEvent(self):
        wm_protocols = self.intern("WM_PROTOCOLS")
        wm_delete_window = self.intern("WM_DELETE_WINDOW")

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

        wid = self.conn.generate_id()
        self.create_window(wid=wid)

        wm_protocols = self.intern("WM_PROTOCOLS")
        wm_delete_window = self.intern("WM_DELETE_WINDOW")

        e = xcffib.xproto.ClientMessageEvent.synthetic(
            format=32,
            window=wid,
            type=wm_protocols,
            data=union
        )

        self.xproto.SendEvent(False, wid, EventMask.NoEvent, e.pack())
        self.conn.flush()

        e = self.conn.wait_for_event()
        assert isinstance(e, xcffib.xproto.ClientMessageEvent)
        assert e.window == wid
        assert list(e.data.data32) == data

    def test_pack_from_event(self):
        wm_protocols = self.intern("WM_PROTOCOLS")
        wm_delete_window = self.intern("WM_DELETE_WINDOW")
        wid = self.conn.generate_id()

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
            format=32,
            window=wid,
            type=wm_protocols,
            data=union
        )

        e2 = xcffib.xproto.ClientMessageEvent(e)

    def test_get_image(self):
        # adapted from: https://gist.github.com/liftoff/4741790
        setup = self.conn.get_setup()
        screen = setup.roots[0]
        width = screen.width_in_pixels
        height = screen.height_in_pixels
        root = screen.root

        # GetImage requires an output format as the first arg.  We want ZPixmap:
        output_format = xcffib.xproto.ImageFormat.ZPixmap
        plane_mask = 2**32 - 1 # No idea what this is but it works!
        reply = self.conn.core.GetImage(
            output_format, root, 0, 0, width, height, plane_mask).reply()
        reply.data.buf()
