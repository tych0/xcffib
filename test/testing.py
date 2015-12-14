# Copyright 2014 Tycho Andersen
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

import xcffib
from xcffib.testing import XvfbTest
from xcffib.xproto import EventMask


class XcffibTest(XvfbTest):
    """ A home for common functions needed for xcffib testing. """

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
            [
                EventMask.SubstructureNotify |
                EventMask.StructureNotify |
                EventMask.SubstructureRedirect
            ]
        )

        self.spawn(['xeyes'])

    def intern(self, name):
        return self.xproto.InternAtom(False, len(name), name).reply().atom
