import xcffib
import struct
import io
MAJOR_VERSION = 1
MINOR_VERSION = 6
key = xcffib.ExtensionKey("RANDR")
_events = {}
_errors = {}
class TRANSFORM(xcffib.Struct):
    xge = False
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Struct.__init__(self, unpacker)
        base = unpacker.offset
        self.matrix11, self.matrix12, self.matrix13, self.matrix21, self.matrix22, self.matrix23, self.matrix31, self.matrix32, self.matrix33 = unpacker.unpack("iiiiiiiii")
        self.bufsize = unpacker.offset - base
    def pack(self):
        buf = io.BytesIO()
        buf.write(struct.pack("=iiiiiiiii", self.matrix11, self.matrix12, self.matrix13, self.matrix21, self.matrix22, self.matrix23, self.matrix31, self.matrix32, self.matrix33))
        return buf.getvalue()
    fixed_size = 36
    @classmethod
    def synthetic(cls, matrix11, matrix12, matrix13, matrix21, matrix22, matrix23, matrix31, matrix32, matrix33):
        self = cls.__new__(cls)
        self.matrix11 = matrix11
        self.matrix12 = matrix12
        self.matrix13 = matrix13
        self.matrix21 = matrix21
        self.matrix22 = matrix22
        self.matrix23 = matrix23
        self.matrix31 = matrix31
        self.matrix32 = matrix32
        self.matrix33 = matrix33
        return self
class GetCrtcTransformReply(xcffib.Reply):
    xge = False
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Reply.__init__(self, unpacker)
        base = unpacker.offset
        unpacker.unpack("xx2x4x")
        self.pending_transform = TRANSFORM(unpacker)
        self.has_transforms, = unpacker.unpack("B3x")
        unpacker.pad(TRANSFORM)
        self.current_transform = TRANSFORM(unpacker)
        self.pending_len, self.pending_nparams, self.current_len, self.current_nparams = unpacker.unpack("4xHHHH")
        unpacker.pad("c")
        self.pending_filter_name = xcffib.List(unpacker, "c", self.pending_len)
        unpacker.pad("i")
        self.pending_params = xcffib.List(unpacker, "i", self.pending_nparams)
        unpacker.pad("c")
        self.current_filter_name = xcffib.List(unpacker, "c", self.current_len)
        unpacker.pad("i")
        self.current_params = xcffib.List(unpacker, "i", self.current_nparams)
        self.bufsize = unpacker.offset - base
class GetCrtcTransformCookie(xcffib.Cookie):
    reply_type = GetCrtcTransformReply
class randrExtension(xcffib.Extension):
    def GetCrtcTransform(self, crtc, is_checked=True):
        buf = io.BytesIO()
        buf.write(struct.pack("=xx2xI", crtc))
        return self.send_request(27, buf, GetCrtcTransformCookie, is_checked=is_checked)
    def GetCrtcTransformUnchecked(self, crtc):
        return self.GetCrtcTransform(crtc, is_checked=False)
xcffib._add_ext(key, randrExtension, _events, _errors)
