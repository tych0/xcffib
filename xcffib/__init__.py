from struct import unpack_from

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

class Struct(ProtoObj):
    pass

class Union(ProtoObj):
    pass

class VoidCookie(ProtoObj):
    pass
