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

import fcntl
import os
import time
import errno
import subprocess

from . import Connection, ConnectionException


def lock_path(display):
    return '/tmp/.X%d-lock' % display


def find_display():
    display = 10
    while True:
        try:
            f = open(lock_path(display), "w+")
            try:
                fcntl.flock(f.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
            except OSError:
                f.close()
                raise
        except OSError:
            display += 1
            continue
        return display, f


class XvfbTest:

    """ A helper class for testing things with nosetests. This class will run
    each test in its own fresh xvfb, leaving you with an xcffib connection to
    that X session as `self.conn` for use in testing. """

    # Set this to true if you'd like to get xtrace output to stdout of each
    # test.
    xtrace = False

    def __init__(self, width=800, height=600, depth=16):
        self.width = width
        self.height = height
        self.depth = depth

    def spawn(self, cmd):
        """ Spawn a command but swallow its output. """
        return subprocess.Popen(cmd)

    def _restore_display(self):
        if self._old_display is None:
            del os.environ['DISPLAY']
        else:
            os.environ['DISPLAY'] = self._old_display

    def setUp(self):
        self._old_display = os.environ.get('DISPLAY')
        self._display, self._display_lock = find_display()
        os.environ['DISPLAY'] = ':%d' % self._display
        self._xvfb = self.spawn(self._xvfb_command())

        if self.xtrace:
            subprocess.Popen(['xtrace', '-n'])
            # xtrace's default display is :9; obviously this won't work
            # concurrently, but it's not the default so...
            os.environ['DISPLAY'] = ':9'
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

        # Delete our X lock file too, since we .kill() the process so it won't
        # clean up after itself.
        try:
            self._display_lock.close()
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
        """ You can override this if you have some extra args for Xvfb or
        whatever. At this point, os.environ['DISPLAY'] is set to something Xvfb
        can use. """
        screen = '%sx%sx%s' % (self.width, self.height, self.depth)
        return ['Xvfb', os.environ['DISPLAY'], '-screen', '0', screen]

    def _connect_to_xvfb(self):
        # sometimes it takes a while for Xvfb to start
        for _ in range(100):
            try:
                conn = Connection(os.environ['DISPLAY'])
                conn.invalid()
                return conn
            except ConnectionException:
                time.sleep(0.2)
        assert False, "couldn't connect to xvfb"
