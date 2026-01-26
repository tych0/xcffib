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

# Not strictly necessary to be included with the binding, but may be useful for
# others who want to test things using xcffib.

import errno
import os
import subprocess
import time

from . import Connection, ConnectionException


def lock_path(display):
    return "/tmp/.X%d-lock" % display


def socket_path(display):
    return "/tmp/.X11-unix/X%d" % display


def _is_pid_alive(pid):
    """Check if a process with the given PID is still running."""
    try:
        os.kill(pid, 0)
        return True
    except OSError as e:
        if e.errno == errno.ESRCH:
            return False
        elif e.errno == errno.EPERM:
            # Process exists but we don't have permission to signal it
            return True
        raise


def _try_acquire_lock(display):
    """Try to atomically create a lock file for the given display.

    Returns (fd, True) if lock acquired, (None, False) otherwise.
    Uses O_CREAT | O_EXCL for atomic creation, which is safe across processes.
    """
    path = lock_path(display)

    # First, check if there's a stale lock file
    try:
        with open(path, "r") as f:
            content = f.read().strip()
            if content:
                try:
                    pid = int(content)
                    if _is_pid_alive(pid):
                        # Lock is held by a running process
                        return None, False
                    # Process is dead, try to remove stale lock
                except ValueError:
                    pass  # Invalid content, try to remove
                try:
                    os.unlink(path)
                except OSError:
                    return None, False
    except FileNotFoundError:
        pass  # No existing lock file, good

    # Try to atomically create the lock file
    try:
        fd = os.open(path, os.O_CREAT | os.O_EXCL | os.O_WRONLY, 0o644)
    except FileExistsError:
        # Another process created it between our check and create
        return None, False
    except OSError:
        return None, False

    # Write our PID to the lock file
    try:
        os.write(fd, ("%d\n" % os.getpid()).encode())
    except OSError:
        os.close(fd)
        try:
            os.unlink(path)
        except OSError:
            pass
        return None, False

    return fd, True


def find_display():
    """Find an available display number and acquire its lock.

    Returns (display_number, lock_fd) on success.
    """
    display = 10
    max_display = 100  # Prevent infinite loop
    while display < max_display:
        fd, acquired = _try_acquire_lock(display)
        if acquired:
            return display, fd
        display += 1
    raise RuntimeError("Could not find an available display (tried :10 through :%d)" % max_display)


class XvfbTest:
    """A helper class for testing things with nosetests. This class will run
    each test in its own fresh xvfb, leaving you with an xcffib connection to
    that X session as `self.conn` for use in testing."""

    # Set this to true if you'd like to get xtrace output to stdout of each
    # test.
    xtrace = False

    def __init__(self, width=800, height=600, depth=16):
        self.width = width
        self.height = height
        self.depth = depth

    def spawn(self, cmd):
        """Spawn a command but swallow its output."""
        return subprocess.Popen(cmd)

    def _restore_display(self):
        if self._old_display is None:
            del os.environ["DISPLAY"]
        else:
            os.environ["DISPLAY"] = self._old_display

    def setUp(self):
        self._old_display = os.environ.get("DISPLAY")
        self._display, self._display_lock = find_display()
        os.environ["DISPLAY"] = ":%d" % self._display
        self._xvfb = self.spawn(self._xvfb_command())

        if self.xtrace:
            subprocess.Popen(["xtrace", "-n"])
            # xtrace's default display is :9; obviously this won't work
            # concurrently, but it's not the default so...
            os.environ["DISPLAY"] = ":9"
        try:
            self.conn = self._connect_to_xvfb()
        except AssertionError:
            self._restore_display()
            raise

    def tearDown(self):
        try:
            self.conn.disconnect()
        except ConnectionException:
            # We don't care if the connection was in an invalid state, maybe
            # the test failed.
            pass
        finally:
            self.conn = None

        self._xvfb.kill()
        self._xvfb.wait()
        self._xvfb = None

        # Wait for the X server socket to be cleaned up before releasing the lock.
        # This prevents another worker from trying to use the same display before
        # the socket is fully released.
        sock_path = socket_path(self._display)
        for _ in range(50):  # Wait up to 5 seconds
            if not os.path.exists(sock_path):
                break
            time.sleep(0.1)

        # Close the lock file descriptor first, then remove the lock file.
        # This order is important: close releases our hold, then we clean up.
        try:
            os.close(self._display_lock)
        except OSError:
            pass

        try:
            os.remove(lock_path(self._display))
        except OSError as e:
            # we don't care if it doesn't exist, maybe something crashed and
            # cleaned it up during a test.
            if e.errno != errno.ENOENT:
                raise
        finally:
            self._restore_display()

    def __enter__(self):
        self.setUp()
        return self

    def __exit__(self, type, value, traceback):
        self.tearDown()

    def _xvfb_command(self):
        """You can override this if you have some extra args for Xvfb or
        whatever. At this point, os.environ['DISPLAY'] is set to something Xvfb
        can use."""
        screen = "%sx%sx%s" % (self.width, self.height, self.depth)
        return ["Xvfb", os.environ["DISPLAY"], "-screen", "0", screen]

    def _connect_to_xvfb(self):
        # sometimes it takes a while for Xvfb to start
        for _ in range(100):
            try:
                conn = Connection(os.environ["DISPLAY"])
                conn.invalid()

                # xvfb creates a screen with a default width, and then resizes it.
                # we wait here for the resize event, so that the actual test only
                # ever sees a screen that is the right size.
                setup = conn.get_setup()
                screen = setup.roots[0]
                if screen.width_in_pixels == self.width and screen.height_in_pixels == self.height:
                    return conn
                conn.disconnect()
            except ConnectionException:
                time.sleep(0.2)
                continue
        assert False, "couldn't connect to xvfb"
