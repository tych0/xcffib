import xcffib
import xcffib.render
import xcffib.xproto
from xcffib.xproto import CW, EventMask, ExposeEvent, WindowClass

WIDTH = 50
HEIGHT = 50


def double_to_fixed(num):
    return int(num * 65536)


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

                # Create picture from the window
                pic_window = conn.generate_id()
                conn.render.CreatePicture(pic_window, window, fmt, 0, [])

                # Create the linear gradient
                pic_gradient = conn.generate_id()
                conn.render.CreateLinearGradientChecked(
                    pic_gradient,
                    xcffib.render.POINTFIX.synthetic(0, 0),
                    xcffib.render.POINTFIX.synthetic(double_to_fixed(WIDTH), double_to_fixed(HEIGHT)),
                    2,
                    [0, double_to_fixed(1)],
                    [
                        xcffib.render.COLOR.synthetic(0, 0, 0, 0xffff),  # Solid black
                        xcffib.render.COLOR.synthetic(0xffff, 0xffff, 0xffff, 0xffff),  # Solid white
                    ],
                ).check()

                # Render the gradient onto the window
                conn.render.Composite(
                    xcffib.render.PictOp.Src,
                    pic_gradient,
                    0,
                    pic_window,
                    0, 0,
                    0, 0,
                    0, 0,
                    WIDTH, HEIGHT
                )

                img = conn.core.GetImage(
                    xcffib.xproto.ImageFormat.ZPixmap,
                    window,
                    0, 0,
                    WIDTH, HEIGHT,
                    0xffffffff
                ).reply()

                conn.flush()

                assert img.data[0:2] == [0, 0]
                assert img.data[-2:] == [255, 255]
                break
