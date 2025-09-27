import xcffib
import struct
import io
_events = {}
_errors = {}
class requestExtension(xcffib.Extension):
    def CreateWindow(self, depth, wid, parent, x, y, width, height, border_width, _class, visual, value_mask, value_list, is_checked=False):
        buf = io.BytesIO()
        buf.write(struct.pack("=xx2xBIIhhHHHHI", depth, wid, parent, x, y, width, height, border_width, _class, visual))
        buf.write(struct.pack("=I", value_mask))
        buf.write(xcffib.pack_list(value_list, "I"))
        return self.send_request(1, buf, is_checked=is_checked)
xcffib._add_ext(key, requestExtension, _events, _errors)
