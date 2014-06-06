import os
import six
import xcffib
from xcffib.ffi import ffi, C
import xcffib.xproto

from nose.tools import raises


class TestConnection(object):

    def setUp(self):
        self.conn = xcffib.Connection(os.environ['DISPLAY'])

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

    """
    # TODO: This probably needs xpybConn_setup implemented to work correctly.
    @raises(xcffib.ConnectionException)
    def test_invalid(self):
        ext = xcffib.xproto.xprotoExtension(self.conn)
        req = six.BytesIO()
        req.write('meshuggah!!1')
        ext.send_request(0, req)
        self.conn.flush()
        print(self.conn.get_maximum_request_length())
        self.conn.invalid()

    def test_list_extensions(self):
        reply = self.conn.core.ListExtensions().reply()
        exts = [''.join(ext.name) for ext in reply.names]
        assert "randr" in exts
    """
