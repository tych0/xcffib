import os
import six
import xcffib
from xcffib.ffi import ffi, C
import xcffib.xproto
from xcffib.xproto import EventMask
from xcffib.testing import XvfbTest

from nose.tools import raises

import struct
import subprocess

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

    def create_window(self, wid=None, x=0, y=0, w=1, h=1, is_checked=False):
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
            ],
            is_checked=is_checked
        )

    def xeyes(self):
        # Enable CreateNotify
        self.xproto.ChangeWindowAttributes(
            self.default_screen.root,
            xcffib.xproto.CW.EventMask,
            [ EventMask.SubstructureNotify |
              EventMask.StructureNotify |
              EventMask.SubstructureRedirect
            ]
        )

        self.spawn(['xeyes'])

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
        exts = [ext.name.to_string() for ext in reply.names]
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
        wid = self.conn.generate_id()
        self.create_window(wid=wid)
        reply = self.xproto.QueryTree(self.default_screen.root).reply()
        assert reply.children_len == 1
        assert len(reply.children) == 1
        assert reply.children[0] == wid

    @raises(AssertionError)
    def test_checking_unchecked_fails(self):
        wid = self.conn.generate_id()
        self.create_window(wid)
        self.xproto.QueryTreeUnchecked(self.default_screen.root).check()

    @raises(AssertionError)
    def test_checking_default_checked_fails(self):
        wid = self.conn.generate_id()
        self.create_window(wid)
        cookie = self.xproto.QueryTree(self.default_screen.root)
        cookie.check()

    def test_checking_foreced_checked_succeeds(self):
        wid = self.conn.generate_id()
        cookie = self.create_window(wid, is_checked=True)
        cookie.check()

    def test_create_window_generates_event(self):
        self.xeyes()
        self.conn.flush()

        e = self.conn.wait_for_event()
        assert isinstance(e, xcffib.xproto.CreateNotifyEvent)

    @raises(xcffib.xproto.WindowError)
    def test_query_invalid_wid_generates_error(self):
        # query a bad WINDOW
        self.xproto.QueryTree(0xf00).reply()

    def test_OpenFont(self):
        fid = self.conn.generate_id()
        self.xproto.OpenFont(fid, len("cursor"), "cursor")

    def test_ConfigureWindow(self):
        wid = self.conn.generate_id()
        self.create_window(wid=wid)
        self.xproto.ConfigureWindowChecked(wid, 0, []).check()

    def test_external_ConfigureWindow(self):
        self.xeyes()
        self.conn.flush()

        e = self.conn.wait_for_event()

        r = self.xproto.ConfigureWindowChecked(e.window, 0, []).check()
        r = self.xproto.DestroyWindowChecked(e.window).check()

    def test_ChangeProperty_WM_NAME(self):
        wid = self.conn.generate_id()
        self.create_window(wid=wid)

        title = "test"
        self.xproto.ChangeProperty(xcffib.xproto.PropMode.Replace, wid,
                xcffib.xproto.Atom.WM_NAME, xcffib.xproto.Atom.STRING, 8,
                len(title), title)

        reply = self.xproto.GetProperty(False, wid,
                xcffib.xproto.Atom.WM_NAME, xcffib.xproto.GetPropertyType.Any, 0, 1).reply()
        assert reply.value.to_string() == title

    def test_ChangeProperty_NET_WM_NAME(self):
        wid = self.conn.generate_id()
        self.create_window(wid=wid)

        title = "test\xc2\xb7"

        net_wm_name = "_NET_WM_NAME"
        net_wm_name = self.xproto.InternAtom(0, len(net_wm_name), net_wm_name).reply().atom

        utf8_string = "UTF8_STRING"
        utf8_string = self.xproto.InternAtom(0, len(utf8_string), utf8_string).reply().atom

        self.xproto.ChangeProperty(xcffib.xproto.PropMode.Replace, wid,
                net_wm_name, utf8_string, 8,
                len(title), title)

        reply = self.xproto.GetProperty(False, wid,
                net_wm_name, xcffib.xproto.GetPropertyType.Any, 0, (2 ** 32) - 1).reply()

        assert reply.value.buf() == six.b("test\xc2\xb7")
        assert reply.value.to_string() == title

    def test_ChangeProperty_WM_PROTOCOLS(self):
        wid = self.conn.generate_id()
        self.create_window(wid=wid)

        wm_protocols = "WM_PROTOCOLS"
        wm_protocols = self.xproto.InternAtom(0, len(wm_protocols), wm_protocols).reply().atom

        wm_delete_window = "WM_DELETE_WINDOW"
        wm_delete_window = self.xproto.InternAtom(0, len(wm_delete_window), wm_delete_window).reply().atom

        self.xproto.ChangeProperty(xcffib.xproto.PropMode.Replace, wid,
                wm_protocols, xcffib.xproto.Atom.ATOM, 32,
                1, (wm_delete_window,))

        reply = self.xproto.GetProperty(False, wid, wm_protocols, xcffib.xproto.Atom.ATOM, 0, 1).reply()

        assert reply.value.to_atoms() == (wm_delete_window,)

        wm_take_focus = "WM_TAKE_FOCUS"
        wm_take_focus = self.xproto.InternAtom(0, len(wm_take_focus), wm_take_focus).reply().atom

        self.xproto.ChangeProperty(xcffib.xproto.PropMode.Replace, wid,
                wm_protocols, xcffib.xproto.Atom.ATOM, 32,
                1, struct.pack("=I", wm_take_focus))

        reply = self.xproto.GetProperty(False, wid, wm_protocols, xcffib.xproto.Atom.ATOM, 0, 1).reply()

        assert reply.value.to_atoms() == (wm_take_focus,)

    def test_GetAtomName(self):
        wm_protocols = "WM_PROTOCOLS"
        atom = self.xproto.InternAtom(0, len(wm_protocols), wm_protocols).reply().atom
        atom_name = self.xproto.GetAtomName(atom).reply().name

        assert atom_name.to_string() == wm_protocols

    def test_KillClient(self):
        self.xeyes()

        self.conn.flush()

        e1 = self.conn.wait_for_event()
        self.xproto.KillClient(e1.window)

        # one is MapRequest and the other is DestroyNotify, they may be in
        # either order
        for _ in range(2):
            self.conn.flush()
            k1 = self.conn.wait_for_event()
            if isinstance(k1, xcffib.xproto.DestroyNotifyEvent):
                assert e1.window == k1.window
                return
        assert False, "no DestroyNotifyEvent"

    def test_connect(self):
        c = xcffib.connect()
        c.invalid()
        c.disconnect()
