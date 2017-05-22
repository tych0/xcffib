# Copyright 2014 Tycho Andersen
# Copyright 2014 Sean Vig
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import os
import six
import xcffib
import xcffib.xproto

from xcffib import ffi
from xcffib.testing import XvfbTest
from .testing import XcffibTest

from nose.tools import raises
from nose import SkipTest

import struct

class TestConnection(XcffibTest):

    def setUp(self):
        XvfbTest.setUp(self)
        self.xproto = xcffib.xproto.xprotoExtension(self.conn)

    def tearDown(self):
        self.xproto = None
        XvfbTest.tearDown(self)

    @raises(xcffib.ConnectionException)
    def test_invalid_display(self):
        self.conn = xcffib.Connection('notvalid')
        self.conn.invalid()

    def test_get_setup(self):
        setup = self.conn.get_setup()
        # When X upgrades, we can probably manage to change this test :-)
        assert setup.protocol_major_version == 11
        assert setup.protocol_minor_version == 0

    def test_get_screen_pointers(self):
        screens = self.conn.get_screen_pointers()
        assert len(screens) == 1
        screen = screens[0]
        assert ffi.typeof(screen) is ffi.typeof("xcb_screen_t *")
        assert screen.root == self.default_screen.root
        assert screen.width_in_pixels == self.width
        assert screen.height_in_pixels == self.height
        assert screen.root_depth == self.depth

    def test_seq_increases(self):
        # If this test starts failing because the sequence numbers don't mach,
        # that's probably because you added a new test that imports a new X
        # extension. When that happens, every new connection automatically does
        # a QueryExtention for each new ext that has been imported, so the
        # squence numbers go up by one.
        #
        # i.e:
        # xproto setup query = seqno 0
        # xtest setup query = seqno 1
        raise SkipTest
        assert self.xproto.GetInputFocus().sequence == 2
        assert self.xproto.GetInputFocus().sequence == 3

    def test_discard_sequence(self):
        cookie = self.xproto.GetInputFocus()
        cookie.discard_reply()
        # this hangs if you leave it in, because the reply really was discarded
        # assert cookie.reply()

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

        cookie = self.xproto.GetGeometry(wid)

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

        net_wm_name = self.intern("_NET_WM_NAME")
        utf8_string = self.intern("UTF8_STRING")

        title_bytes = b"test\xc2\xb7"
        title_string = six.u("test\u00B7")

        # First check with an object already encoded as bytes
        self.xproto.ChangeProperty(xcffib.xproto.PropMode.Replace, wid,
                net_wm_name, utf8_string, 8,
                len(title_bytes), title_bytes)

        reply = self.xproto.GetProperty(False, wid,
                net_wm_name, xcffib.xproto.GetPropertyType.Any, 0, (2 ** 32) - 1).reply()

        print(reply.value.buf())
        assert reply.value.buf() == title_bytes
        print(reply.value.to_utf8())
        assert reply.value.to_utf8() == title_string

        # Also check with a unicode string
        self.xproto.ChangeProperty(xcffib.xproto.PropMode.Replace, wid,
                net_wm_name, utf8_string, 8,
                len(title_string.encode('utf-8')), title_string)

        reply = self.xproto.GetProperty(False, wid,
                net_wm_name, xcffib.xproto.GetPropertyType.Any, 0, (2 ** 32) - 1).reply()

        assert reply.value.buf() == title_bytes
        assert reply.value.to_utf8() == title_string

    def test_ChangeProperty_WM_PROTOCOLS(self):
        wid = self.conn.generate_id()
        self.create_window(wid=wid)

        wm_protocols = self.intern("WM_PROTOCOLS")

        wm_delete_window = self.intern("WM_DELETE_WINDOW")

        self.xproto.ChangeProperty(xcffib.xproto.PropMode.Replace, wid,
                wm_protocols, xcffib.xproto.Atom.ATOM, 32,
                1, (wm_delete_window,))
        # For Python 2 only, make sure packing can handle both ints and longs
        if six.PY2:
            self.xproto.ChangeProperty(xcffib.xproto.PropMode.Replace, wid,
                    wm_protocols, xcffib.xproto.Atom.ATOM, 32,
                    1, (long(wm_delete_window),))

        reply = self.xproto.GetProperty(False, wid, wm_protocols, xcffib.xproto.Atom.ATOM, 0, 1).reply()

        assert reply.value.to_atoms() == (wm_delete_window,)

        wm_take_focus = self.intern("WM_TAKE_FOCUS")

        self.xproto.ChangeProperty(xcffib.xproto.PropMode.Replace, wid,
                wm_protocols, xcffib.xproto.Atom.ATOM, 32,
                1, (wm_take_focus,))

        reply = self.xproto.GetProperty(False, wid, wm_protocols, xcffib.xproto.Atom.ATOM, 0, 1).reply()

        assert reply.value.to_atoms() == (wm_take_focus,)

    def test_GetAtomName(self):
        wm_protocols = "WM_PROTOCOLS"
        atom = self.intern(wm_protocols)
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
        assert c.has_error() == 0
        c.disconnect()

    def test_auth_connect(self):
        authname = six.b("MIT-MAGIC-COOKIE-1")
        authdata = six.b("\xa5\xcf\x95\xfa\x19\x49\x03\x60\xaf\xe4\x1e\xcd\xa3\xe2\xad\x47")

        authstr = authname + six.b(':') + authdata

        conn = xcffib.connect(display=os.environ['DISPLAY'], auth=authstr)

        assert conn.get_setup().roots[0].root > 0

    # This is an adaptation of the test from #27
    def test_build_atom_cache(self):
        # This will hold the forward *and* reverse lookups for any given atom
        atoms = {}
        cookies = []
        # Batch the replies by creating a list of cookies first:
        for i in range(1, 10000):
            c = self.conn.core.GetAtomName(i)
            cookies.append((i, c))
        for i, c in cookies:
            try:
                name = c.reply().name.to_string()
            except xcffib.xproto.BadAtom:
                continue
            atoms.update({i: name}) # Lookup by number
            atoms.update({name: i}) # Lookup by name

    def test_wrap(self):
        c = xcffib.connect()
        c.invalid()
        c2 = xcffib.wrap(xcffib.ffi.cast("long", c._conn))
        c2.invalid()
        c2.disconnect()
