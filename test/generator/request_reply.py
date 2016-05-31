import xcffib
import struct
import io
_events = {}
_errors = {}
class STR(xcffib.Struct):
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Struct.__init__(self, unpacker)
        base = unpacker.offset
        self.name_len, = unpacker.unpack("B")
        self.name = xcffib.List(unpacker, "c", self.name_len)
        self.bufsize = unpacker.offset - base
    def pack(self):
        buf = io.BytesIO()
        buf.write(struct.pack("=B", self.name_len))
        buf.write(xcffib.pack_list(self.name, "c"))
        return buf.getvalue()
    @classmethod
    def synthetic(cls, name_len, name):
        self = cls.__new__(cls)
        self.name_len = name_len
        self.name = name
        return self
class ListExtensionsReply(xcffib.Reply):
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Reply.__init__(self, unpacker)
        base = unpacker.offset
        self.names_len, = unpacker.unpack("xB2x4x24x")
        self.names = xcffib.List(unpacker, STR, self.names_len)
        self.bufsize = unpacker.offset - base
class ListExtensionsCookie(xcffib.Cookie):
    reply_type = ListExtensionsReply
class request_replyExtension(xcffib.Extension):
    def ListExtensions(self, is_checked=True):
        buf = io.BytesIO()
        buf.write(struct.pack("=xx2x"))
        return self.send_request(99, buf, ListExtensionsCookie, is_checked=is_checked)
xcffib._add_ext(key, request_replyExtension, _events, _errors)
