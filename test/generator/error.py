import xcffib
import struct
import io
MAJOR_VERSION = 2
MINOR_VERSION = 2
key = xcffib.ExtensionKey("ERROR")
_events = {}
_errors = {}
class RequestError(xcffib.Error):
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Error.__init__(self, unpacker)
        base = unpacker.offset
        self.bad_value, self.minor_opcode, self.major_opcode = unpacker.unpack("xx2xIHBx")
        self.bufsize = unpacker.offset - base
    def pack(self):
        buf = io.BytesIO()
        buf.write(struct.pack("=B", 1))
        buf.write(struct.pack("=x2xIHBx", self.bad_value, self.minor_opcode, self.major_opcode))
        return buf.getvalue()
BadRequest = RequestError
_errors[1] = RequestError
xcffib._add_ext(key, errorExtension, _events, _errors)
