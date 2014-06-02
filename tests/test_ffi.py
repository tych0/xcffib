import six
from xcffib.ffi import ffi, bytes_to_cdata

def test_bytes_to_cdata():
    bs = six.b('these are some bytes')
    assert bs == bytes(ffi.buffer(bytes_to_cdata(bs), len(bs)))
