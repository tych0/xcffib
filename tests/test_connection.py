import os
import six
import xcffib
from xcffib.ffi import ffi, C
import xcffib.xproto

from nose.tools import raises


class TestConnection(object):

    def setUp(self):
        self.conn = xcffib.Connection(os.environ['DISPLAY'].encode('utf-8'))

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
