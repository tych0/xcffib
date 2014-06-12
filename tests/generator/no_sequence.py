import xcffib
import struct
import six
_events = {}
_errors = {}
class KeymapNotifyEvent(xcffib.Event):
    def __init__(self, parent, offset, size):
        xcffib.Event.__init__(self, parent, offset, size)
        base = offset
        offset += 1
        self.keys = xcffib.List(parent, offset, 31, "B", 1)
        offset += self.keys.bufsize
        self.bufsize = offset - base
_events[11] = KeymapNotifyEvent
xcffib._add_ext(xcffib.ExtensionKey("no_sequence"), no_sequenceExtension, _events, _errors)
