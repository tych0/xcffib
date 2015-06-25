import xcffib
import struct
import six
_events = {}
_errors = {}
class INT64(xcffib.Struct):
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Struct.__init__(self, unpacker)
        base = unpacker.offset
        self.hi, self.lo = unpacker.unpack("iI")
        self.bufsize = unpacker.offset - base
    def pack(self):
        buf = six.BytesIO()
        buf.write(struct.pack("=iI", self.hi, self.lo))
        return buf.getvalue()
    fixed_size = 8
class GetPropertyReply(xcffib.Reply):
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Reply.__init__(self, unpacker)
        base = unpacker.offset
        self.num_items, self.format = unpacker.unpack("xx2x4xIB")
        if self.format == PropertyFormat._8Bits:
            self.items = xcffib.List(unpacker, "B", self.num_items)
        elif self.format == PropertyFormat._16Bits:
            self.items = xcffib.List(unpacker, "H", self.num_items)
        elif self.format == PropertyFormat._32Bits:
            self.items = xcffib.List(unpacker, "I", self.num_items)
        self.bufsize = unpacker.offset - base
class GetPropertyCookie(xcffib.Cookie):
    reply_type = GetPropertyReply
class GetPropertyWithPadReply(xcffib.Reply):
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Reply.__init__(self, unpacker)
        base = unpacker.offset
        self.num_items, self.format = unpacker.unpack("xx2x4xIB")
        self.names = xcffib.List(unpacker, "B", self.num_items)
        if self.format == PropertyFormat._8Bits:
            unpacker.pad("B")
            self.items = xcffib.List(unpacker, "B", self.num_items)
        elif self.format == PropertyFormat._16Bits:
            unpacker.pad("H")
            self.items = xcffib.List(unpacker, "H", self.num_items)
        elif self.format == PropertyFormat._32Bits:
            unpacker.pad("I")
            self.items = xcffib.List(unpacker, "I", self.num_items)
        self.bufsize = unpacker.offset - base
class GetPropertyWithPadCookie(xcffib.Cookie):
    reply_type = GetPropertyWithPadReply
class switchExtension(xcffib.Extension):
    def GetProperty(self, value_mask, items, is_checked=True):
        buf = six.BytesIO()
        buf.write(struct.pack("=xx2xI", value_mask))
        if value_mask == CA.Counter:
            buf.write(buf.write(struct.pack("=I", items)))
        elif value_mask == CA.Value:
            buf.write(items.pack())
        elif value_mask == CA.ValueType:
            buf.write(buf.write(struct.pack("=I", items)))
        elif value_mask == CA.Events:
            buf.write(buf.write(struct.pack("=I", items)))
        return self.send_request(59, buf, GetPropertyCookie, is_checked=is_checked)
    def GetPropertyWithPad(self, is_checked=True):
        buf = six.BytesIO()
        buf.write(struct.pack("=xx2x"))
        return self.send_request(60, buf, GetPropertyWithPadCookie, is_checked=is_checked)
xcffib._add_ext(key, switchExtension, _events, _errors)
