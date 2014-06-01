import xcffib
import struct
import cStringIO
_events = {}
_errors = {}
class STR(xcffib.Struct):
    def __init__(self, parent, offset, size):
        xcffib.Struct.__init__(self, parent, offset, size)
        self.name_len, = struct.unpack_from("B", parent, offset)
        offset += 1
        self.name = xcffib.List(parent, offset, self.name_len, "b", 1)
        offset += self.name.bufsize
class ListExtensionsReply(xcffib.Reply):
    def __init__(self, parent, offset, size):
        xcffib.Reply.__init__(self, parent, offset, size)
        self.names_len, = struct.unpack_from("B24x", parent, offset)
        offset += 25
        self.names = xcffib.List(parent, offset, self.names_len, STR)
        offset += self.names.bufsize
class ListExtensionsCookie(xcffib.Cookie):
    pass
class request_replyExtension(xcffib.Extension):
    def ListExtensions(self):
        buf = cStringIO.StringIO()
        return self.send_request(99, buf)
xcffib._add_ext(xcffib.ExtensionKey("request_reply"), request_replyExtension, _events, _errors)
