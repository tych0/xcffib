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
import xcffib
import xcffib.xproto

from xcffib import ffi

import pytest


class TestConnection:
    def test_invalid_display(self, xproto_test):
        with pytest.raises(xcffib.ConnectionException):
            xproto_test.conn = xcffib.Connection("notvalid")

    def test_get_setup(self, xproto_test):
        setup = xproto_test.conn.get_setup()
        # When X upgrades, we can probably manage to change this test :-)
        assert setup.protocol_major_version == 11
        assert setup.protocol_minor_version == 0

    def test_get_screen_pointers(self, xproto_test):
        screens = xproto_test.conn.get_screen_pointers()
        assert len(screens) == 1
        screen = screens[0]
        assert ffi.typeof(screen) is ffi.typeof("xcb_screen_t *")
        assert screen.root == xproto_test.default_screen.root
        assert screen.width_in_pixels == xproto_test.width
        assert screen.height_in_pixels == xproto_test.height
        assert screen.root_depth == xproto_test.depth

    def test_seq_increases(self, xproto_test):
        # If this test starts failing because the sequence numbers don't mach,
        # that's probably because you added a new test that imports a new X
        # extension. When that happens, every new connection automatically does
        # a QueryExtention for each new ext that has been imported, so the
        # squence numbers go up by one.
        #
        # i.e:
        # xproto setup query = seqno 0
        # xtest setup query = seqno 1
        assert xproto_test.xproto.GetInputFocus().sequence == 2
        assert xproto_test.xproto.GetInputFocus().sequence == 3

    def test_discard_sequence(self, xproto_test):
        cookie = xproto_test.xproto.GetInputFocus()
        cookie.discard_reply()
        # this hangs if you leave it in, because the reply really was discarded
        # assert cookie.reply()

    def test_invalid(self, xproto_test):
        with pytest.raises(xcffib.ConnectionException):
            xcffib.Connection("notadisplay")

    def test_list_extensions(self, xproto_test):
        reply = xproto_test.conn.core.ListExtensions().reply()
        exts = [ext.name.to_string() for ext in reply.names]
        assert "XVideo" in exts

    def test_create_window(self, xproto_test):
        wid = xproto_test.conn.generate_id()
        cookie = xproto_test.create_window(wid=wid)

        cookie = xproto_test.xproto.GetGeometry(wid)

        reply = cookie.reply()
        assert reply.x == 0
        assert reply.y == 0
        assert reply.width == 1
        assert reply.height == 1

    def test_wait_for_nonexistent_request(self, xproto_test):
        with pytest.raises(xcffib.XcffibException):
            xproto_test.conn.wait_for_reply(10)

    def test_no_windows(self, xproto_test):
        # Make sure there aren't any windows in the root window. This mostly
        # just exists to make sure people aren't somehow mistakenly running a
        # test in their own X session, which could corrupt results.
        reply = xproto_test.xproto.QueryTree(xproto_test.default_screen.root).reply()
        assert reply.children_len == 0
        assert len(reply.children) == 0

    def test_create_window_creates_window(self, xproto_test):
        wid = xproto_test.conn.generate_id()
        xproto_test.create_window(wid=wid)
        reply = xproto_test.xproto.QueryTree(xproto_test.default_screen.root).reply()
        assert reply.children_len == 1
        assert len(reply.children) == 1
        assert reply.children[0] == wid

    def test_checking_unchecked_fails(self, xproto_test):
        with pytest.raises(AssertionError):
            wid = xproto_test.conn.generate_id()
            xproto_test.create_window(wid)
            xproto_test.xproto.QueryTreeUnchecked(
                xproto_test.default_screen.root
            ).check()

    def test_checking_default_checked_fails(self, xproto_test):
        with pytest.raises(AssertionError):
            wid = xproto_test.conn.generate_id()
            xproto_test.create_window(wid)
            cookie = xproto_test.xproto.QueryTree(xproto_test.default_screen.root)
            cookie.check()

    def test_checking_foreced_checked_succeeds(self, xproto_test):
        wid = xproto_test.conn.generate_id()
        cookie = xproto_test.create_window(wid, is_checked=True)
        cookie.check()

    def test_create_window_generates_event(self, xproto_test):
        xproto_test.xeyes()

        e = xproto_test.conn.wait_for_event()
        assert isinstance(e, xcffib.xproto.CreateNotifyEvent)

    def test_query_invalid_wid_generates_error(self, xproto_test):
        with pytest.raises(xcffib.xproto.WindowError):
            # query a bad WINDOW
            xproto_test.xproto.QueryTree(0xF00).reply()

    def test_OpenFont(self, xproto_test):
        fid = xproto_test.conn.generate_id()
        xproto_test.xproto.OpenFont(fid, len("cursor"), "cursor")

    def test_ConfigureWindow(self, xproto_test):
        wid = xproto_test.conn.generate_id()
        xproto_test.create_window(wid=wid)
        xproto_test.xproto.ConfigureWindowChecked(wid, 0, []).check()

    def test_external_ConfigureWindow(self, xproto_test):
        xproto_test.xeyes()

        e = xproto_test.conn.wait_for_event()

        xproto_test.xproto.ConfigureWindowChecked(e.window, 0, []).check()
        xproto_test.xproto.DestroyWindowChecked(e.window).check()

    def test_ChangeProperty_WM_NAME(self, xproto_test):
        wid = xproto_test.conn.generate_id()
        xproto_test.create_window(wid=wid)

        title = "test"
        xproto_test.xproto.ChangeProperty(
            xcffib.xproto.PropMode.Replace,
            wid,
            xcffib.xproto.Atom.WM_NAME,
            xcffib.xproto.Atom.STRING,
            8,
            len(title),
            title,
        )

        reply = xproto_test.xproto.GetProperty(
            False,
            wid,
            xcffib.xproto.Atom.WM_NAME,
            xcffib.xproto.GetPropertyType.Any,
            0,
            1,
        ).reply()
        assert reply.value.to_string() == title

    def test_ChangeProperty_NET_WM_NAME(self, xproto_test):
        wid = xproto_test.conn.generate_id()
        xproto_test.create_window(wid=wid)

        net_wm_name = xproto_test.intern("_NET_WM_NAME")
        utf8_string = xproto_test.intern("UTF8_STRING")

        title_bytes = b"test\xc2\xb7"
        title_string = "test\u00B7"

        # First check with an object already encoded as bytes
        xproto_test.xproto.ChangeProperty(
            xcffib.xproto.PropMode.Replace,
            wid,
            net_wm_name,
            utf8_string,
            8,
            len(title_bytes),
            title_bytes,
        )

        reply = xproto_test.xproto.GetProperty(
            False, wid, net_wm_name, xcffib.xproto.GetPropertyType.Any, 0, (2 ** 32) - 1
        ).reply()

        print(reply.value.buf())
        assert reply.value.buf() == title_bytes
        print(reply.value.to_utf8())
        assert reply.value.to_utf8() == title_string

        # Also check with a unicode string
        xproto_test.xproto.ChangeProperty(
            xcffib.xproto.PropMode.Replace,
            wid,
            net_wm_name,
            utf8_string,
            8,
            len(title_string.encode("utf-8")),
            title_string,
        )

        reply = xproto_test.xproto.GetProperty(
            False, wid, net_wm_name, xcffib.xproto.GetPropertyType.Any, 0, (2 ** 32) - 1
        ).reply()

        assert reply.value.buf() == title_bytes
        assert reply.value.to_utf8() == title_string

    def test_ChangeProperty_WM_PROTOCOLS(self, xproto_test):
        wid = xproto_test.conn.generate_id()
        xproto_test.create_window(wid=wid)

        wm_protocols = xproto_test.intern("WM_PROTOCOLS")

        wm_delete_window = xproto_test.intern("WM_DELETE_WINDOW")

        xproto_test.xproto.ChangeProperty(
            xcffib.xproto.PropMode.Replace,
            wid,
            wm_protocols,
            xcffib.xproto.Atom.ATOM,
            32,
            1,
            (wm_delete_window,),
        )

        reply = xproto_test.xproto.GetProperty(
            False, wid, wm_protocols, xcffib.xproto.Atom.ATOM, 0, 1
        ).reply()

        assert reply.value.to_atoms() == (wm_delete_window,)

        wm_take_focus = xproto_test.intern("WM_TAKE_FOCUS")

        xproto_test.xproto.ChangeProperty(
            xcffib.xproto.PropMode.Replace,
            wid,
            wm_protocols,
            xcffib.xproto.Atom.ATOM,
            32,
            1,
            (wm_take_focus,),
        )

        reply = xproto_test.xproto.GetProperty(
            False, wid, wm_protocols, xcffib.xproto.Atom.ATOM, 0, 1
        ).reply()

        assert reply.value.to_atoms() == (wm_take_focus,)

    def test_GetAtomName(self, xproto_test):
        wm_protocols = "WM_PROTOCOLS"
        atom = xproto_test.intern(wm_protocols)
        atom_name = xproto_test.xproto.GetAtomName(atom).reply().name

        assert atom_name.to_string() == wm_protocols

    def test_KillClient(self, xproto_test):
        xproto_test.xeyes()

        e1 = xproto_test.conn.wait_for_event()
        xproto_test.xproto.KillClient(e1.window)

        # one is MapRequest and the other is DestroyNotify, they may be in
        # either order
        for _ in range(2):
            xproto_test.conn.flush()
            k1 = xproto_test.conn.wait_for_event()
            if isinstance(k1, xcffib.xproto.DestroyNotifyEvent):
                assert e1.window == k1.window
                return
        assert False, "no DestroyNotifyEvent"

    def test_connect(self, xproto_test):
        c = xcffib.connect()
        c.invalid()
        assert c.has_error() == 0
        c.disconnect()

    def test_auth_connect(self, xproto_test):
        authname = b"MIT-MAGIC-COOKIE-1"
        authdata = b"\xa5\xcf\x95\xfa\x19\x49\x03\x60\xaf\xe4\x1e\xcd\xa3\xe2\xad\x47"

        authstr = authname + b":" + authdata

        conn = xcffib.connect(display=os.environ["DISPLAY"], auth=authstr)

        assert conn.get_setup().roots[0].root > 0

    # This is an adaptation of the test from #27
    def test_build_atom_cache(self, xproto_test):
        # This will hold the forward *and* reverse lookups for any given atom
        atoms = {}
        cookies = []
        # Batch the replies by creating a list of cookies first:
        for i in range(1, 10000):
            c = xproto_test.conn.core.GetAtomName(i)
            cookies.append((i, c))
        for i, c in cookies:
            try:
                name = c.reply().name.to_string()
            except xcffib.xproto.BadAtom:
                continue
            atoms.update({i: name})  # Lookup by number
            atoms.update({name: i})  # Lookup by name

    def test_wrap(self, xproto_test):
        c = xcffib.connect()
        c.invalid()
        c2 = xcffib.wrap(xcffib.ffi.cast("long", c._conn))
        c2.invalid()
        c2.disconnect()
