import xcffib
import struct
import six
MAJOR_VERSION = 1
MINOR_VERSION = 4
key = xcffib.ExtensionKey("EVENT")
_events = {}
_errors = {}
class ScreenChangeNotifyEvent(xcffib.Event):
    struct_length = 32
    def __init__(self, parent, offset):
        xcffib.Event.__init__(self, parent, offset)
        base = offset
        self.rotation, self.timestamp, self.config_timestamp, self.root, self.request_window, self.sizeID, self.subpixel_order, self.width, self.height, self.mwidth, self.mheight = struct.unpack_from("xB2xIIIIHHHHHH", parent, offset)
        offset += 32
        self.bufsize = offset - base
_events[0] = ScreenChangeNotifyEvent
xcffib._add_ext(xcffib.ExtensionKey("event"), eventExtension, _events, _errors)
