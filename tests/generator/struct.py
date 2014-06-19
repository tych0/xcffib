import xcffib
import struct
import six
_events = {}
_errors = {}
class AxisInfo(xcffib.Struct):
    def __init__(self, parent, offset, size):
        xcffib.Struct.__init__(self, parent, offset, size)
        base = offset
        self.resolution, self.minimum, self.maximum = struct.unpack_from("Iii", parent, offset)
        offset += 12
        self.bufsize = offset - base
    def pack(self):
        buf = six.BytesIO()
        buf.write(struct.pack("=Iii", self.resolution, self.minimum, self.maximum))
        return buf.getvalue()
class ValuatorInfo(xcffib.Struct):
    def __init__(self, parent, offset, size):
        xcffib.Struct.__init__(self, parent, offset, size)
        base = offset
        self.class_id, self.len, self.axes_len, self.mode, self.motion_size = struct.unpack_from("BBBBI", parent, offset)
        offset += 8
        self.axes = xcffib.List(parent, offset, self.axes_len, AxisInfo, 12)
        offset += self.axes.bufsize
        self.bufsize = offset - base
    def pack(self):
        buf = six.BytesIO()
        buf.write(struct.pack("=BBBBI", self.class_id, self.len, self.axes_len, self.mode, self.motion_size))
        buf.write(xcffib.pack_list(self.axes, AxisInfo, self.axes_len))
        return buf.getvalue()
xcffib._add_ext(key, structExtension, _events, _errors)
