import xcffib
import struct
import io
from dataclasses import dataclass
_events = {}
_errors = {}
@dataclass(init=False)
class SANoAction(xcffib.Struct):
    xge = False
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Struct.__init__(self, unpacker)
        base = unpacker.offset
        self.type, = unpacker.unpack("B7x")
        self.bufsize = unpacker.offset - base
    def pack(self):
        buf = io.BytesIO()
        buf.write(struct.pack("=B7x", self.type))
        return buf.getvalue()
    fixed_size = 8
    @classmethod
    def synthetic(cls, type):
        self = cls.__new__(cls)
        self.type = type
        return self
@dataclass(init=False)
class SASetMods(xcffib.Struct):
    xge = False
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Struct.__init__(self, unpacker)
        base = unpacker.offset
        self.type, self.flags, self.mask, self.realMods, self.vmodsHigh, self.vmodsLow = unpacker.unpack("BBBBBB2x")
        self.bufsize = unpacker.offset - base
    def pack(self):
        buf = io.BytesIO()
        buf.write(struct.pack("=BBBBBB2x", self.type, self.flags, self.mask, self.realMods, self.vmodsHigh, self.vmodsLow))
        return buf.getvalue()
    fixed_size = 8
    @classmethod
    def synthetic(cls, type, flags, mask, realMods, vmodsHigh, vmodsLow):
        self = cls.__new__(cls)
        self.type = type
        self.flags = flags
        self.mask = mask
        self.realMods = realMods
        self.vmodsHigh = vmodsHigh
        self.vmodsLow = vmodsLow
        return self
@dataclass(init=False)
class SASetGroup(xcffib.Struct):
    xge = False
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Struct.__init__(self, unpacker)
        base = unpacker.offset
        self.type, self.flags, self.group = unpacker.unpack("BBb5x")
        self.bufsize = unpacker.offset - base
    def pack(self):
        buf = io.BytesIO()
        buf.write(struct.pack("=BBb5x", self.type, self.flags, self.group))
        return buf.getvalue()
    fixed_size = 8
    @classmethod
    def synthetic(cls, type, flags, group):
        self = cls.__new__(cls)
        self.type = type
        self.flags = flags
        self.group = group
        return self
@dataclass(init=False)
class SAMovePtrFlag:
    NoAcceleration = 1 << 0
    MoveAbsoluteX = 1 << 1
    MoveAbsoluteY = 1 << 2
@dataclass(init=False)
class SAMovePtr(xcffib.Struct):
    xge = False
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Struct.__init__(self, unpacker)
        base = unpacker.offset
        self.type, self.flags, self.xHigh, self.xLow, self.yHigh, self.yLow = unpacker.unpack("BBbBbB2x")
        self.bufsize = unpacker.offset - base
    def pack(self):
        buf = io.BytesIO()
        buf.write(struct.pack("=BBbBbB2x", self.type, self.flags, self.xHigh, self.xLow, self.yHigh, self.yLow))
        return buf.getvalue()
    fixed_size = 8
    @classmethod
    def synthetic(cls, type, flags, xHigh, xLow, yHigh, yLow):
        self = cls.__new__(cls)
        self.type = type
        self.flags = flags
        self.xHigh = xHigh
        self.xLow = xLow
        self.yHigh = yHigh
        self.yLow = yLow
        return self
@dataclass(init=False)
class SAPtrBtn(xcffib.Struct):
    xge = False
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Struct.__init__(self, unpacker)
        base = unpacker.offset
        self.type, self.flags, self.count, self.button = unpacker.unpack("BBBB4x")
        self.bufsize = unpacker.offset - base
    def pack(self):
        buf = io.BytesIO()
        buf.write(struct.pack("=BBBB4x", self.type, self.flags, self.count, self.button))
        return buf.getvalue()
    fixed_size = 8
    @classmethod
    def synthetic(cls, type, flags, count, button):
        self = cls.__new__(cls)
        self.type = type
        self.flags = flags
        self.count = count
        self.button = button
        return self
@dataclass(init=False)
class SALockPtrBtn(xcffib.Struct):
    xge = False
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Struct.__init__(self, unpacker)
        base = unpacker.offset
        self.type, self.flags, self.button = unpacker.unpack("BBxB4x")
        self.bufsize = unpacker.offset - base
    def pack(self):
        buf = io.BytesIO()
        buf.write(struct.pack("=BBxB4x", self.type, self.flags, self.button))
        return buf.getvalue()
    fixed_size = 8
    @classmethod
    def synthetic(cls, type, flags, button):
        self = cls.__new__(cls)
        self.type = type
        self.flags = flags
        self.button = button
        return self
@dataclass(init=False)
class Action(xcffib.Union):
    xge = False
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Union.__init__(self, unpacker)
        self.noaction = SANoAction(unpacker.copy())
        self.setmods = SASetMods(unpacker.copy())
        self.lockmods = SASetMods(unpacker.copy())
        self.setgroup = SASetGroup(unpacker.copy())
        self.lockgroup = SASetGroup(unpacker.copy())
        self.moveptr = SAMovePtr(unpacker.copy())
        self.ptrbtn = SAPtrBtn(unpacker.copy())
        self.lockptrbtn = SALockPtrBtn(unpacker.copy())
        self.type, = unpacker.copy().unpack("B")
    def pack(self):
        buf = io.BytesIO()
        buf.write(self.noaction.pack() if hasattr(self.noaction, "pack") else SANoAction.synthetic(*self.noaction).pack())
        return buf.getvalue()
xcffib._add_ext(key, actionExtension, _events, _errors)
