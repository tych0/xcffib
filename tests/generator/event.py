import xcffib
import struct
import cStringIO
MAJOR_VERSION = 1
MINOR_VERSION = 4
key = xcffib.ExtensionKey("EVENT")
_events = {}
_errors = {}
class ScreenChangeNotifyEvent(xcffib.Event):
    def __init__(self, parent, offset, size):
        xcffib.Event.__init__(self, parent, offset, size)
        self.rotation, self.timestamp, self.config_timestamp, self.root, self.request_window, self.sizeID, self.subpixel_order, self.width, self.height, self.mwidth, self.mheight = struct.unpack_from("BIIIIHHHHHH", parent, offset)
        offset += 29
_events[0] = ScreenChangeNotifyEvent
xcffib._add_ext(xcffib.ExtensionKey("event"), eventExtension, _events, _errors)
