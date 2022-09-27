import xcffib
import struct
import io
_events = {}
_errors = {}
class CHARINFO(xcffib.Struct):
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Struct.__init__(self, unpacker)
        base = unpacker.offset
        self.left_side_bearing, self.right_side_bearing, self.character_width, self.ascent, self.descent, self.attributes = unpacker.unpack("hhhhhH")
        self.bufsize = unpacker.offset - base
    def pack(self):
        buf = io.BytesIO()
        buf.write(struct.pack("=hhhhhH", self.left_side_bearing, self.right_side_bearing, self.character_width, self.ascent, self.descent, self.attributes))
        return buf.getvalue()
    fixed_size = 12
    @classmethod
    def synthetic(cls, left_side_bearing, right_side_bearing, character_width, ascent, descent, attributes):
        self = cls.__new__(cls)
        self.left_side_bearing = left_side_bearing
        self.right_side_bearing = right_side_bearing
        self.character_width = character_width
        self.ascent = ascent
        self.descent = descent
        self.attributes = attributes
        return self
class FONTPROP(xcffib.Struct):
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Struct.__init__(self, unpacker)
        base = unpacker.offset
        self.name, self.value = unpacker.unpack("II")
        self.bufsize = unpacker.offset - base
    def pack(self):
        buf = io.BytesIO()
        buf.write(struct.pack("=II", self.name, self.value))
        return buf.getvalue()
    fixed_size = 8
    @classmethod
    def synthetic(cls, name, value):
        self = cls.__new__(cls)
        self.name = name
        self.value = value
        return self
class ListFontsWithInfoReply(xcffib.Reply):
    def __init__(self, unpacker):
        if isinstance(unpacker, xcffib.Protobj):
            unpacker = xcffib.MemoryUnpacker(unpacker.pack())
        xcffib.Reply.__init__(self, unpacker)
        base = unpacker.offset
        self.name_len, = unpacker.unpack("xB2x4x")
        self.min_bounds = CHARINFO(unpacker)
        unpacker.unpack("4x")
        unpacker.pad(CHARINFO)
        self.max_bounds = CHARINFO(unpacker)
        self.min_char_or_byte2, self.max_char_or_byte2, self.default_char, self.properties_len, self.draw_direction, self.min_byte1, self.max_byte1, self.all_chars_exist, self.font_ascent, self.font_descent, self.replies_hint = unpacker.unpack("4xHHHHBBBBhhI")
        unpacker.pad(FONTPROP)
        self.properties = xcffib.List(unpacker, FONTPROP, self.properties_len)
        unpacker.pad("c")
        self.name = xcffib.List(unpacker, "c", self.name_len)
        self.bufsize = unpacker.offset - base
class ListFontsWithInfoCookie(xcffib.Cookie):
    reply_type = ListFontsWithInfoReply
class type_padExtension(xcffib.Extension):
    def ListFontsWithInfo(self, max_names, pattern_len, pattern, is_checked=True):
        buf = io.BytesIO()
        buf.write(struct.pack("=xx2xxHH", max_names, pattern_len))
        buf.write(xcffib.pack_list(pattern, "c"))
        return self.send_request(50, buf, ListFontsWithInfoCookie, is_checked=is_checked)
xcffib._add_ext(key, type_padExtension, _events, _errors)
