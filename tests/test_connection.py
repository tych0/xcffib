import os
import six
import xcffib
from xcffib.ffi import ffi, C
import xcffib.xproto

from nose.tools import raises


class TestConnection(object):

    def setUp(self):
        self.conn = xcffib.Connection(os.environ['DISPLAY'])
        self.xproto = xcffib.xproto.xprotoExtension(self.conn)

    def tearDown(self):
        try:
            self.conn.disconnect()
        except xcffib.ConnectionException:
            pass
        finally:
            self.conn = None

    def test_connect(self):
        assert self.conn.has_error() == 0

    @raises(xcffib.ConnectionException)
    def test_invalid_display(self):
        self.conn = xcffib.Connection('notvalid')
        self.conn.invalid()

    def test_get_setup(self):
        setup = self.conn.get_setup()
        # When X upgrades, we can probably manage to change this test :-)
        assert setup.protocol_major_version == 11
        assert setup.protocol_minor_version == 0

    def test_seq_increases(self):
        assert self.xproto.GetInputFocus().sequence == 1
        assert self.xproto.GetInputFocus().sequence == 2

    @raises(xcffib.ConnectionException)
    def test_invalid(self):
        conn = xcffib.Connection('notadisplay')
        conn.invalid()

    def test_list_extensions(self):
        reply = self.conn.core.ListExtensions().reply()
        print(reply.names_len)
        print(reply.names)
        exts = [''.join(map(chr, ext.name)) for ext in reply.names]
        assert "XVideo" in exts
