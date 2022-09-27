import xcffib
import struct
import io
_events = {}
_errors = {}
class ClientMessageData(xcffib.Union):
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Union.__init__(self, unpacker)
        self.data8 = xcffib.List(unpacker.copy(), "B", 20)
        self.data16 = xcffib.List(unpacker.copy(), "H", 10)
        self.data32 = xcffib.List(unpacker.copy(), "I", 5)
    def pack(self):
        buf = io.BytesIO()
        buf.write(xcffib.pack_list(self.data8, "B"))
        return buf.getvalue()
xcffib._add_ext(key, unionExtension, _events, _errors)
