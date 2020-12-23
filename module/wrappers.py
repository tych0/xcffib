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
import weakref


def IDWrapper(finalizer):
    """
    Classes create with IDWrapper return an ID and then free it upon exit of
    the context or when garbage collected, whichever comes first.
    """
    class Wrapper:
        def __init__(self, conn):
            super().__init__()
            self.id = conn.generate_id()
            _finalizer = None

            def finalize(conn, xid):
                _finalizer.detach()
                getattr(conn.core, finalizer)(xid)

            _finalizer = weakref.finalize(
                self,
                finalize,
                conn,
                self.id,
            )
            self._finalizer = _finalizer

        def __enter__(self):
            return self.id

        def __exit__(self, exception_type, exception_value, traceback):
            self.finalize()

        def finalize(self):
            self._finalizer()

    return Wrapper


PixmapID = IDWrapper("FreePixmap")
GContextID = IDWrapper("FreeGC")
ColormapID = IDWrapper("FreeColormap")
CursorID = IDWrapper("FreeCursor")
