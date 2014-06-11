import xcffib
import struct
import six
MAJOR_VERSION = 2
MINOR_VERSION = 2
key = xcffib.ExtensionKey("ERROR")
_events = {}
_errors = {}
class RequestError(xcffib.Error):
    def __init__(self, parent, offset, size):
        xcffib.Error.__init__(self, parent, offset, size)
        base = offset
        self.bad_value, self.minor_opcode, self.major_opcode = struct.unpack_from("xx2xIHBx", parent, offset)
        offset += 12
        self.bufsize = offset - base
_events[1] = RequestError
xcffib._add_ext(xcffib.ExtensionKey("error"), errorExtension, _events, _errors)
