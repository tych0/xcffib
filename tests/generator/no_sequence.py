import xcffib
import struct
import six
_events = {}
_errors = {}
class KeymapNotifyEvent(xcffib.Event):
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Event.__init__(self, unpacker)
        base = unpacker.offset
        unpacker.unpack("x")
        self.keys = xcffib.List(unpacker, "B", 31)
        self.bufsize = unpacker.offset - base
    def pack(self):
        buf = six.BytesIO()
        buf.write(struct.pack("=B", 11))
        buf.write(xcffib.pack_list(self.keys, "B"))
        buf_len = len(buf.getvalue())
        if buf_len < 32:
            buf.write(struct.pack("x" * (32 - buf_len)))
        return buf.getvalue()
    def synthetic(self, keys):
        self.keys = keys
_events[11] = KeymapNotifyEvent
xcffib._add_ext(key, no_sequenceExtension, _events, _errors)
