# Copyright 2012 Florian Mounier
# Copyright 2014 Sean Vig
# Copyright 2014 Tycho Andersen
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

"""
This is mostly stolen from qtile's tests/scripts/window.py
"""

import os
import sys
import struct
import time
import xcffib
import xcffib.xproto
from xcffib.xproto import EventMask


class TestWindow:
    def test_the_script(self, xcffib_test):
        NAME = "one"
        for i in range(20):
            try:
                conn = xcffib.connect(os.environ["DISPLAY"])
            except xcffib.ConnectionException:
                time.sleep(0.1)
                continue
            except Exception as v:
                print("Error opening test window: ", type(v), v, file=sys.stderr)
                sys.exit(1)
            break
        else:
            print(
                "Could not open window on display %s" % (sys.argv[1]), file=sys.stderr
            )
            sys.exit(1)

        screen = conn.get_setup().roots[conn.pref_screen]

        window = conn.generate_id()
        background = (
            conn.core.AllocColor(screen.default_colormap, 0x2828, 0x8383, 0xCECE)
            .reply()
            .pixel
        )  # Color "#2883ce"
        conn.core.CreateWindow(
            xcffib.CopyFromParent,
            window,
            screen.root,
            100,
            100,
            100,
            100,
            1,
            xcffib.xproto.WindowClass.InputOutput,
            screen.root_visual,
            xcffib.xproto.CW.BackPixel | xcffib.xproto.CW.EventMask,
            [
                background,
                xcffib.xproto.EventMask.StructureNotify
                | xcffib.xproto.EventMask.Exposure,
            ],
        )

        conn.core.ChangeProperty(
            xcffib.xproto.PropMode.Replace,
            window,
            xcffib.xproto.Atom.WM_NAME,
            xcffib.xproto.Atom.STRING,
            8,
            len(NAME),
            NAME,
        )

        wm_protocols = "WM_PROTOCOLS"
        wm_protocols = (
            conn.core.InternAtom(0, len(wm_protocols), wm_protocols).reply().atom
        )

        wm_delete_window = "WM_DELETE_WINDOW"
        wm_delete_window = (
            conn.core.InternAtom(0, len(wm_delete_window), wm_delete_window)
            .reply()
            .atom
        )

        conn.core.ChangeProperty(
            xcffib.xproto.PropMode.Replace,
            window,
            wm_protocols,
            xcffib.xproto.Atom.ATOM,
            32,
            1,
            [wm_delete_window],
        )

        conn.core.ConfigureWindow(
            window,
            xcffib.xproto.ConfigWindow.X
            | xcffib.xproto.ConfigWindow.Y
            | xcffib.xproto.ConfigWindow.Width
            | xcffib.xproto.ConfigWindow.Height
            | xcffib.xproto.ConfigWindow.BorderWidth,
            [0, 0, 100, 100, 1],
        )
        conn.core.MapWindow(window)
        conn.flush()
        conn.core.ConfigureWindow(
            window,
            xcffib.xproto.ConfigWindow.X
            | xcffib.xproto.ConfigWindow.Y
            | xcffib.xproto.ConfigWindow.Width
            | xcffib.xproto.ConfigWindow.Height
            | xcffib.xproto.ConfigWindow.BorderWidth,
            [0, 0, 100, 100, 1],
        )

        # now kill the window from the "wm" side via WM_DELETE_WINDOW protocol
        WM_PROTOCOLS = (
            xcffib_test.conn.core.InternAtom(False, len("WM_PROTOCOLS"), "WM_PROTOCOLS")
            .reply()
            .atom
        )
        WM_DELETE_WINDOW = (
            xcffib_test.conn.core.InternAtom(
                False, len("WM_DELETE_WINDOW"), "WM_DELETE_WINDOW"
            )
            .reply()
            .atom
        )
        vals = [
            33,  # ClientMessageEvent
            32,  # Format
            0,
            window,
            WM_PROTOCOLS,
            WM_DELETE_WINDOW,
            xcffib.xproto.Time.CurrentTime,
            0,
            0,
            0,
        ]
        e = struct.pack("BBHII5I", *vals)
        xcffib_test.conn.core.SendEvent(False, window, EventMask.NoEvent, e)

        xcffib_test.conn.flush()

        while 1:
            conn.flush()
            event = conn.wait_for_event()
            if event.__class__ == xcffib.xproto.ClientMessageEvent:
                atom = conn.core.GetAtomName(event.type).reply().name.to_string()
                print(atom)
                if atom == "WM_PROTOCOLS":
                    break
        # This test passes if it gets all the way here without dying :-)
