import os
import xcffib
import xcffib.xproto
from xcffib.xproto import CW, EventMask, ExposeEvent, KeyPressEvent, WindowClass
import xcffib.render
from xcffib.wrappers import PixmapID

from xcffib import ffi

import pytest


def find_format(screen, depth, visual):
    for d in screen.depths:
        if d.depth == depth:
            for v in d.visuals:
                if v.visual == visual:
                    return v.format


class TestConnection:
    def test_CreateLinearGradient(self, xproto_test):
        conn = xproto_test.conn
        setup = conn.get_setup()
        root = setup.roots[0].root
        depth = setup.roots[0].root_depth
        visual = setup.roots[0].root_visual
        black = setup.roots[0].black_pixel
        conn.render = conn(xcffib.render.key)
        conn.render.QueryVersion(xcffib.render.MAJOR_VERSION, xcffib.render.MINOR_VERSION)

        window = conn.generate_id()
        conn.core.CreateWindow(
            depth,
            window,
            root,
            0,
            0,
            640,
            480,
            0,
            WindowClass.InputOutput,
            visual,
            CW.BackPixel | CW.EventMask,
            [black, EventMask.Exposure | EventMask.KeyPress],
        )

        conn.core.MapWindow(window)
        conn.flush()

        while True:
            event = conn.wait_for_event()
            if isinstance(event, ExposeEvent):
                cookie = conn.render.QueryPictFormats()
                reply = cookie.reply()
                fmt = find_format(reply.screens[0], depth, visual)

                with PixmapID(conn) as pix_mask:
                    # Create a pixmap and picture for the mask
                    conn.core.CreatePixmap(depth, pix_mask, window, 200, 200)
                    pic_mask = conn.generate_id()
                    conn.render.CreatePicture(pic_mask, pix_mask, fmt, 0, [])

                    # Create the linear gradient
                    pic_gradient = conn.generate_id()

                    # This gives an "xcffib.xproto.LengthError"...
                    conn.render.CreateLinearGradientChecked(
                        pic_gradient,
                        xcffib.render.POINTFIX.synthetic(0, 0),
                        xcffib.render.POINTFIX.synthetic(200, 200),
                        2,
                        [0, 1],
                        [
                            xcffib.render.COLOR.synthetic(0, 0, 65535, 65535),
                            xcffib.render.COLOR.synthetic(0, 65535, 65535, 65535),
                        ],
                    ).check()
                    break
