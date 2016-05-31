import xcffib
import struct
import io
MAJOR_VERSION = 0
MINOR_VERSION = 11
key = xcffib.ExtensionKey("RENDER")
_events = {}
_errors = {}
class COLOR(xcffib.Struct):
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Struct.__init__(self, unpacker)
        base = unpacker.offset
        self.red, self.green, self.blue, self.alpha = unpacker.unpack("HHHH")
        self.bufsize = unpacker.offset - base
    def pack(self):
        buf = io.BytesIO()
        buf.write(struct.pack("=HHHH", self.red, self.green, self.blue, self.alpha))
        return buf.getvalue()
    fixed_size = 8
    @classmethod
    def synthetic(cls, red, green, blue, alpha):
        self = cls.__new__(cls)
        self.red = red
        self.green = green
        self.blue = blue
        self.alpha = alpha
        return self
class RECTANGLE(xcffib.Struct):
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Struct.__init__(self, unpacker)
        base = unpacker.offset
        self.x, self.y, self.width, self.height = unpacker.unpack("hhHH")
        self.bufsize = unpacker.offset - base
    def pack(self):
        buf = io.BytesIO()
        buf.write(struct.pack("=hhHH", self.x, self.y, self.width, self.height))
        return buf.getvalue()
    fixed_size = 8
    @classmethod
    def synthetic(cls, x, y, width, height):
        self = cls.__new__(cls)
        self.x = x
        self.y = y
        self.width = width
        self.height = height
        return self
class renderExtension(xcffib.Extension):
    def FillRectangles(self, op, dst, color, rects_len, rects, is_checked=False):
        buf = io.BytesIO()
        buf.write(struct.pack("=xx2xB3xI", op, dst))
        buf.write(color.pack() if hasattr(color, "pack") else COLOR.synthetic(*color).pack())
        buf.write(xcffib.pack_list(rects, RECTANGLE))
        return self.send_request(26, buf, is_checked=is_checked)
xcffib._add_ext(key, renderExtension, _events, _errors)
