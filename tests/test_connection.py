import os
import six
import xcffib
from xcffib.ffi import ffi, C
import xcffib.xproto
from xcffib.testing import XvfbTest

from nose.tools import raises


class TestConnection(XvfbTest):

    def setUp(self):
        XvfbTest.setUp(self)
        self.xproto = xcffib.xproto.xprotoExtension(self.conn)

    def tearDown(self):
        self.xproto = None
        XvfbTest.tearDown(self)

    @property
    def default_screen(self):
        return self.conn.setup.roots[self.conn.pref_screen]


    def create_window(self, wid=None, x=0, y=0, w=1, h=1):
        if wid is None:
            wid = self.conn.generate_id()
        return self.xproto.CreateWindow(
            self.default_screen.root_depth,
            wid,
            self.default_screen.root,
            x, y, w, h,
            0,
            xcffib.xproto.WindowClass.InputOutput,
            self.default_screen.root_visual,
            xcffib.xproto.CW.BackPixel | xcffib.xproto.CW.EventMask,
            [
                self.default_screen.black_pixel,
                xcffib.xproto.EventMask.StructureNotify
            ]
        )

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
        exts = [''.join(map(chr, ext.name)) for ext in reply.names]
        assert "XVideo" in exts

    def test_create_window(self):
        wid = self.conn.generate_id()
        cookie = self.create_window(wid=wid)
        assert cookie.sequence == 1

        cookie = self.xproto.GetGeometry(wid)
        assert cookie.sequence == 2

        reply = cookie.reply()
        assert reply.x == 0
        assert reply.y == 0
        assert reply.width == 1
        assert reply.height == 1

    @raises(xcffib.XcffibException)
    def test_wait_for_nonexistent_request(self):
        self.conn.wait_for_reply(10)

    def test_no_windows(self):
        # Make sure there aren't any windows in the root window. This mostly
        # just exists to make sure people aren't somehow mistakenly running a
        # test in their own X session, which could corrupt results.
        reply = self.xproto.QueryTree(self.default_screen.root).reply()
        assert reply.children_len == 0
        assert len(reply.children) == 0

    def test_create_window_creates_window(self):
        self.create_window()
        reply = self.xproto.QueryTree(self.default_screen.root).reply()
        assert reply.children_len == 1
        assert len(reply.children) == 1

