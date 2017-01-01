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
    @classmethod
    def synthetic(cls, hi, lo):
        self = cls.__new__(cls)
        self.hi = hi
        self.lo = lo
        return self
class GetPropertyReply(xcffib.Reply):
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Reply.__init__(self, unpacker)
        base = unpacker.offset
        self.num_items, self.format = unpacker.unpack("xx2x4xIB")
        if self.format & PropertyFormat._8Bits:
            self.data8 = xcffib.List(unpacker, "B", self.num_items)
        if self.format & PropertyFormat._16Bits:
            self.data16 = xcffib.List(unpacker, "H", self.num_items)
        if self.format & PropertyFormat._32Bits:
            self.data32 = xcffib.List(unpacker, "I", self.num_items)
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
        if self.format & PropertyFormat._8Bits:
            unpacker.pad("B")
            self.data8 = xcffib.List(unpacker, "B", self.num_items)
        if self.format & PropertyFormat._16Bits:
            unpacker.pad("H")
            self.data16 = xcffib.List(unpacker, "H", self.num_items)
        if self.format & PropertyFormat._32Bits:
            unpacker.pad("I")
            self.data32 = xcffib.List(unpacker, "I", self.num_items)
        self.bufsize = unpacker.offset - base
class GetPropertyWithPadCookie(xcffib.Cookie):
    reply_type = GetPropertyWithPadReply
class switchExtension(xcffib.Extension):
    def GetProperty(self, value_mask, items, is_checked=True):
        buf = six.BytesIO()
        buf.write(struct.pack("=xx2xI", value_mask))
        if value_mask & CA.Counter:
            counter = items.pop(0)
            buf.write(struct.pack("=I", counter))
        if value_mask & CA.Value:
            value = items.pop(0)
            buf.write(value.pack() if hasattr(value, "pack") else INT64.synthetic(*value).pack())
        if value_mask & CA.ValueType:
            valueType = items.pop(0)
            buf.write(struct.pack("=I", valueType))
        if value_mask & CA.Events:
            events = items.pop(0)
            buf.write(struct.pack("=I", events))
        return self.send_request(59, buf, GetPropertyCookie, is_checked=is_checked)
    def GetPropertyWithPad(self, is_checked=True):
        buf = six.BytesIO()
        buf.write(struct.pack("=xx2x"))
        return self.send_request(60, buf, GetPropertyWithPadCookie, is_checked=is_checked)
xcffib._add_ext(key, switchExtension, _events, _errors)
