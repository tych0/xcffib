from struct import unpack_from

from ffi import *

# For xpyb compatibility
NONE = XCB_NONE
CopyFromParent = XCB_COPY_FROM_PARENT
CurrentTime = XCB_CURRENT_TIME
NoSymbol = XCB_NO_SYMBOL

class Exception(object):
    pass

class ConnectException(object):
    pass

class ExtensionException(object):
    pass

class ProtocolException(object):
    pass

class ExtensionKey(object):
    """ This definitely isn't needed, but we keep it around for compatibilty
    with xpyb.
    """
    def __init__(self, name):
        self.name = name

    def __hash__(self):
        return hash(self.name)
    def __eq__(self, o):
        return self.name == o.name
    def __ne__(self, o):
        return self.name != o.name

class Extension(object):
    # TODO: implement
    pass

class ProtoObj(object):

    """ Note: Unlike xcb.ProtoObj, this does NOT implement the sequence
    protocol. I found this behavior confusing: ProtoObj would implement the
    sequence protocol on self.buf, and then List would go and implement it on
    List. Additionally, as near as I can tell internally we only need the size
    of the buffer for cases when the size of things is unspecified. Thus,
    that's all we save.
    """

    def __init__(self, parent, offset, size=None):
        """
        Params:
        - parent: a bytes()
        - offset: the start of this offest in the bytes()
        - size: the size of this object (if none, then it is assumed to be
          len(parent))

        I don't actually think we need the size parameter here at all, but xpyb has
        it so we keep it around.
        """

        assert len(parent) < offset
        if size is not None:
            assert len(parent) > size + offset
        else:
            size = len(parent)
        self.bufsize = size - offset

class List(ProtoObj):
    def __init__(self, parent, offset, length, typ, size=-1):

        if size > 0:
            assert len(parent) > length * size + offset

        self.list = []
        cur = offset

        if isinstance(typ, str):
            self.list = list(unpack_from(typ * length, parent, offset))
        elif size > 0:
            for _ in range(length):
                self.list.append(typ(parent, cur, size))
                cur += size
        else:
            for _ in range(length):
                item = typ(parent, cur)
                cur += item.bufsize

    def __len__(self):
        return len(self.list)
    # TODO: implement the rest of the sequence protocol

# These thre are all empty.
class Struct(ProtoObj):
    pass

class Union(ProtoObj):
    pass

class VoidCookie(ProtoObj):
    pass

class Connection(object):
    def __init__(self, display=None, fd=-1, auth=None):
        if auth != None:
            c_auth = C.new("xcb_auth_info_t *")
            if C.xpyb_parse_auth(auth, len(auth), auth_out) < 0:
                raise Exception("invalid xauth")
        else:
            c_auth = C.NULL


        i = C.new("int *")

        if fd > 0:
            self._conn = C.xcb_connect_to_fd(fd, c_auth)
        else if c_auth != C.NULL:
            self._conn = C.xcb_connect_to_display_with_auth(display, c_auth, i)
        else:
            self._conn = C.xcb_connect(display, i)
        self.pref_screen = ffi.int(i)

        self.core = core(self)
        # TODO: xpybConn_setup

core = None
core_events = None
core_errors = None
setup = None

# This seems a bit over engineered to me; it seems unlikely there will ever be
# a core besides xproto, so why not just hardcode that?
def _add_core(value, setup, events, errors):
    if not isinstance(value, Extension):
        raise Exception("Extension type not derived from xcffib.Extension")
    if not isinstance(setup, Struct):
        raise Exception("Setup type not derived from xcffib.Struct")

    global core
    global core_events
    global core_errors
    global setup

    core = value
    core_events = events
    core_errors = errors
    setup = setup
