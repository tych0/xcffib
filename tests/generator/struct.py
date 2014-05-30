import xcffib
import struct
import cStringIO
_events = {}
_errors = {}
class AxisInfo(xcffib.Protobj):
    def __init__(self, parent, offset, size):
        xcffib.Protobj.__init__(self, parent, offset, size)
        self.resolution, self.minimum, self.maximum = struct.unpack_from("Iii", parent, offset)
        offset += 12
class ValuatorInfo(xcffib.Protobj):
    def __init__(self, parent, offset, size):
        xcffib.Protobj.__init__(self, parent, offset, size)
        self.class_id, self.len, self.axes_len, self.mode, self.motion_size = struct.unpack_from("BBBBI", parent, offset)
        offset += 8
        self.axes = xcffib.List(parent, offset, self.axes_len, AxisInfo, 12)
        offset += self.axes.bufsize
