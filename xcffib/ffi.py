from cffi import FFI

# TODO: don't require a c compiler at runtime :-)
# http://cffi.readthedocs.org/en/release-0.8/index.html?highlight=ffilibrary#distributing-modules-using-cffi

ffi = FFI()

constants = [
    "X_PROTOCOL",
    "X_PROTOCOL_REVISION",
    "X_TCP_PORT",

    "XCB_NONE",
    "XCB_COPY_FROM_PARENT",
    "XCB_CURRENT_TIME",
    "XCB_NO_SYMBOL",

    "XCB_CONN_ERROR",
    "XCB_CONN_CLOSED_EXT_NOTSUPPORTED",
    "XCB_CONN_CLOSED_MEM_INSUFFICIENT",
    "XCB_CONN_CLOSED_REQ_LEN_EXCEED",
    "XCB_CONN_CLOSED_PARSE_ERR",
    "XCB_CONN_CLOSED_INVALID_SCREEN",
    "XCB_CONN_CLOSED_FDPASSING_FAILED",
]

ffi.cdef('\n'.join("#define %s ..." % c for c in constants))

C = ffi.verify("""
    #include <xcb/xcb.h>
    #include <xcb/xcbext.h>
""")

for c in constants:
    globals()[c] = getattr(C, c)
