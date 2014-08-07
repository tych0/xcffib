import xcffib
import struct
import six
_events = {}
_errors = {}
class ClientMessageData(xcffib.Union):
    def __init__(self, unpacker):
        xcffib.Union.__init__(self, unpacker)
        self.data8 = xcffib.List(unpacker.copy(), "B", 20)
        self.data16 = xcffib.List(unpacker.copy(), "H", 10)
        self.data32 = xcffib.List(unpacker.copy(), "I", 5)
xcffib._add_ext(key, unionExtension, _events, _errors)
