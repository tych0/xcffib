import xcffib
import struct
import six
_events = {}
_errors = {}
class requestExtension(xcffib.Extension):
    def CreateWindow(self, depth, wid, parent, x, y, width, height, border_width, _class, visual, value_mask, value_list):
        buf = six.BytesIO()
        buf.write(struct.pack("BIIhhHHHHI", depth, wid, parent, x, y, width, height, border_width, _class, visual, value_mask, value_list))
        buf.write(struct.pack("I", value_mask) + xcffib.pack_list(value_list, "I", 4))
        return self.send_request(1, buf)
xcffib._add_ext(xcffib.ExtensionKey("request"), requestExtension, _events, _errors)
