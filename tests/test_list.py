""" Test list packing and unpacking. """

import xcffib
import struct

class TestList(object):

    def test_struct_pack_uses_List(self):
        # suppose we have a list of ints...
        ints = struct.pack("=IIII", *range(4))

        # Unpacker wants a cffi.cdata
        cffi_ints = xcffib.bytes_to_cdata(ints)

        l = xcffib.List(xcffib.Unpacker(cffi_ints), "I", count=4)
        ints2 = struct.pack("=IIII", *l)

        # after packing and unpacking, we should still have those ints
        assert ints == ints2
