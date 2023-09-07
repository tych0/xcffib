import xcffib
import struct
import io
MAJOR_VERSION = 1
MINOR_VERSION = 13
key = xcffib.ExtensionKey("RECORD")
_events = {}
_errors = {}
class Range8(xcffib.Struct):
    xge = False
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Struct.__init__(self, unpacker)
        base = unpacker.offset
        self.first, self.last = unpacker.unpack("BB")
        self.bufsize = unpacker.offset - base
    def pack(self):
        buf = io.BytesIO()
        buf.write(struct.pack("=BB", self.first, self.last))
        return buf.getvalue()
    fixed_size = 2
    @classmethod
    def synthetic(cls, first, last):
        self = cls.__new__(cls)
        self.first = first
        self.last = last
        return self
class Range16(xcffib.Struct):
    xge = False
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Struct.__init__(self, unpacker)
        base = unpacker.offset
        self.first, self.last = unpacker.unpack("HH")
        self.bufsize = unpacker.offset - base
    def pack(self):
        buf = io.BytesIO()
        buf.write(struct.pack("=HH", self.first, self.last))
        return buf.getvalue()
    fixed_size = 4
    @classmethod
    def synthetic(cls, first, last):
        self = cls.__new__(cls)
        self.first = first
        self.last = last
        return self
class ExtRange(xcffib.Struct):
    xge = False
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Struct.__init__(self, unpacker)
        base = unpacker.offset
        self.major = Range8(unpacker)
        unpacker.pad(Range16)
        self.minor = Range16(unpacker)
        self.bufsize = unpacker.offset - base
    def pack(self):
        buf = io.BytesIO()
        buf.write(self.major.pack() if hasattr(self.major, "pack") else Range8.synthetic(*self.major).pack())
        buf.write(self.minor.pack() if hasattr(self.minor, "pack") else Range16.synthetic(*self.minor).pack())
        return buf.getvalue()
    @classmethod
    def synthetic(cls, major, minor):
        self = cls.__new__(cls)
        self.major = major
        self.minor = minor
        return self
class Range(xcffib.Struct):
    xge = False
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Struct.__init__(self, unpacker)
        base = unpacker.offset
        self.core_requests = Range8(unpacker)
        unpacker.pad(Range8)
        self.core_replies = Range8(unpacker)
        unpacker.pad(ExtRange)
        self.ext_requests = ExtRange(unpacker)
        unpacker.pad(ExtRange)
        self.ext_replies = ExtRange(unpacker)
        unpacker.pad(Range8)
        self.delivered_events = Range8(unpacker)
        unpacker.pad(Range8)
        self.device_events = Range8(unpacker)
        unpacker.pad(Range8)
        self.errors = Range8(unpacker)
        self.client_started, self.client_died = unpacker.unpack("BB")
        self.bufsize = unpacker.offset - base
    def pack(self):
        buf = io.BytesIO()
        buf.write(self.core_requests.pack() if hasattr(self.core_requests, "pack") else Range8.synthetic(*self.core_requests).pack())
        buf.write(self.core_replies.pack() if hasattr(self.core_replies, "pack") else Range8.synthetic(*self.core_replies).pack())
        buf.write(self.ext_requests.pack() if hasattr(self.ext_requests, "pack") else ExtRange.synthetic(*self.ext_requests).pack())
        buf.write(self.ext_replies.pack() if hasattr(self.ext_replies, "pack") else ExtRange.synthetic(*self.ext_replies).pack())
        buf.write(self.delivered_events.pack() if hasattr(self.delivered_events, "pack") else Range8.synthetic(*self.delivered_events).pack())
        buf.write(self.device_events.pack() if hasattr(self.device_events, "pack") else Range8.synthetic(*self.device_events).pack())
        buf.write(self.errors.pack() if hasattr(self.errors, "pack") else Range8.synthetic(*self.errors).pack())
        buf.write(struct.pack("=B", self.client_started))
        buf.write(struct.pack("=B", self.client_died))
        return buf.getvalue()
    @classmethod
    def synthetic(cls, core_requests, core_replies, ext_requests, ext_replies, delivered_events, device_events, errors, client_started, client_died):
        self = cls.__new__(cls)
        self.core_requests = core_requests
        self.core_replies = core_replies
        self.ext_requests = ext_requests
        self.ext_replies = ext_replies
        self.delivered_events = delivered_events
        self.device_events = device_events
        self.errors = errors
        self.client_started = client_started
        self.client_died = client_died
        return self
xcffib._add_ext(key, recordExtension, _events, _errors)
