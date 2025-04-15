import xcffib
import struct
import io
from dataclasses import dataclass
_events = {}
_errors = {}
@dataclass(init=False)
class EventForSend(xcffib.Buffer):
    pass
@dataclass(init=False)
class eventstructExtension(xcffib.Extension):
    def SendExtensionEvent(self, device_id, propagate, num_classes, num_events, events, classes, is_checked=False):
        buf = io.BytesIO()
        buf.write(struct.pack("=xx2xBBHB3x", device_id, propagate, num_classes, num_events))
        buf.write(xcffib.pack_list(events, EventForSend))
        buf.write(xcffib.pack_list(classes, "B"))
        return self.send_request(31, buf, is_checked=is_checked)
xcffib._add_ext(key, eventstructExtension, _events, _errors)
