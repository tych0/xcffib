import os
import xcffib
import xcffib.xproto

class TestConnection(object):

    def setUp(self):
        self.conn = xcffib.Connection(os.environ['DISPLAY'])

    def tearDown(self):
        self.conn.disconnect()
        self.conn = None

    def test_connect(self):
        assert self.conn.has_error() == 0

    """
    def test_request_version(self):
        print(xcffib.__file__)
        reply = self.conn.core.ListExtensions().reply()
        exts = [''.join(ext.name) for ext in reply.names]
        assert "randr" in exts
    """
