import xcffib
import struct
import six
_events = {}
_errors = {}
class STR(xcffib.Struct):
    def __init__(self, parent, offset, size):
        xcffib.Struct.__init__(self, parent, offset, size)
        base = offset
        self.name_len, = struct.unpack_from("B", parent, offset)
        offset += 1
        self.name = xcffib.List(parent, offset, self.name_len, "b", 1)
        offset += self.name.bufsize
        self.bufsize = offset - base
class ListExtensionsReply(xcffib.Reply):
    def __init__(self, parent, offset, size):
        xcffib.Reply.__init__(self, parent, offset, size)
        base = offset
        self.names_len, = struct.unpack_from("xB2x4x24x", parent, offset)
        offset += 32
        self.names = xcffib.List(parent, offset, self.names_len, STR)
        offset += self.names.bufsize
        self.bufsize = offset - base
class ListExtensionsCookie(xcffib.Cookie):
    reply_type = ListExtensionsReply
class request_replyExtension(xcffib.Extension):
    def ListExtensions(self):
        buf = six.BytesIO()
        buf.write(struct.pack("xx2x"))
        return self.send_request(99, buf, ListExtensionsCookie)
xcffib._add_ext(xcffib.ExtensionKey("request_reply"), request_replyExtension, _events, _errors)
