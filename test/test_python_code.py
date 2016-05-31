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
from xcffib.xproto import EventMask

from .testing import XcffibTest

from nose.tools import raises

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

        assert cm.data32[0] == 0x03020100
        assert cm.data32[1] == 0x07060504
        assert cm.data32[2] == 0x0b0a0908

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

    def test_pack_list_string(self):
        data = [[(-2, -1, 0, 1, 2), "i"], [(1.1, 2.2, 3.3), "d"]]
        for d, f in data:
            packed = xcffib.pack_list(d, f)
            assert d == struct.unpack("=%s%s" % (len(d), f), packed)

    def test_pack_list_struct(self):
        class FooStructTest(StructTest):
            pass

        class BarStructTest(StructTest):
            pass

        class UselessStructTest(StructTest):
            def pack(self): pass
            synthetic = classmethod(lambda self: self.__new__(self))

        data = [FooStructTest.synthetic("3I3i", (1, 2, 3, -5, -6, -7)),
                BarStructTest.synthetic("3d", (1.1, 2.2, 3.3))]

        def pack_list_test_struct(struct_list, pack_type):
            packed = xcffib.pack_list(struct_list, pack_type)
            unpacker = xcffib.MemoryUnpacker(packed)
            for s in struct_list:
                assert type(s)(s.fmt, unpacker) == s
            assert unpacker.offset == unpacker.known_max

        pack_list_test_struct(data, UselessStructTest)

    def test_pack_list_synthetic(self):
        data_synthetic = [["3I3i", (1, 2, 3, -5, -6, -7)],
                          ["3d", (1.1, 2.2, 3.3)]]

        def pack_list_test_synthetic(iterable_list, pack_type):
            packed = xcffib.pack_list(iterable_list, pack_type)
            unpacker = xcffib.MemoryUnpacker(packed)
            for i in iterable_list:
                assert pack_type.synthetic(*i) == pack_type(i[0], unpacker)
            assert unpacker.offset == unpacker.known_max

        pack_list_test_synthetic(data_synthetic, StructTest)

    def test_pack_list_packed(self):
        data_packed = [b"one", b"two", b"three"]

        def pack_list_test_packed(packed_list):
            packed_a = xcffib.pack_list(packed_list, None)
            packed_b = b"".join(b for b in packed_list)
            assert packed_a == packed_b

        pack_list_test_packed(data_packed)

    @raises(TypeError)
    def test_pack_list_unpacked(self):
        data_unpacked = [object(), [1, 2, 3]]
        xcffib.pack_list(data_unpacked, None)


class StructTest(xcffib.Struct):
    def __init__(self, fmt, unpacker):
        xcffib.Struct.__init__(self, unpacker)
        self.data = unpacker.unpack(fmt)
        self.fmt = fmt

    def __eq__(self, other):
        if isinstance(other, type(self)):
            return (self.fmt == other.fmt and
                    self.data == other.data)
        else:
            return NotImplemented

    def pack(self):
        return struct.pack("=" + self.fmt, *self.data)

    @classmethod
    def synthetic(cls, fmt, data):
        self = cls.__new__(cls)
        self.fmt = fmt
        self.data = data
        return self
