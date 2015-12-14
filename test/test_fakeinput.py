import xcffib
import xcffib.xproto
import xcffib.xtest

from .testing import XcffibTest

class TestConnection(XcffibTest):
    def test_fakeinput(self):
        xtest = self.conn(xcffib.xtest.key)

        setup = self.conn.get_setup()
        screen = setup.roots[0]
        def test(x, y):
            # motion
            xtest.FakeInput(
                6,
                0,
                xcffib.xproto.Time.CurrentTime,
                screen.root,
                x,
                y,
                0)

            # press
            xtest.FakeInput(
                4,
                1,
                xcffib.xproto.Time.CurrentTime,
                screen.root,
                0,
                0,
                0)

            # release
            xtest.FakeInput(
                5,
                1,
                xcffib.xproto.Time.CurrentTime,
                screen.root,
                2,
                2,
                0)
            self.conn.flush()
        test(50, 10)

        # we shouldn't get any errors
        self.conn.poll_for_event()
