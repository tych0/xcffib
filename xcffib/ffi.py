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


# constants
ffi.cdef('\n'.join("#define %s ..." % c for c in constants))

# types
ffi.cdef("""
    typedef struct {
        uint8_t   response_type;  /**< Type of the response */
        uint8_t  pad0;           /**< Padding */
        uint16_t sequence;       /**< Sequence number */
        uint32_t length;         /**< Length of the response */
    } xcb_generic_reply_t;

    typedef struct {
        uint8_t   response_type;  /**< Type of the response */
        uint8_t  pad0;           /**< Padding */
        uint16_t sequence;       /**< Sequence number */
        uint32_t pad[7];         /**< Padding */
        uint32_t full_sequence;  /**< full sequence */
    } xcb_generic_event_t;

    typedef struct {
        uint8_t   response_type;  /**< Type of the response */
        uint8_t   error_code;     /**< Error code */
        uint16_t sequence;       /**< Sequence number */
        uint32_t resource_id;     /** < Resource ID for requests with side effects only */
        uint16_t minor_code;      /** < Minor opcode of the failed request */
        uint8_t major_code;       /** < Major opcode of the failed request */
        uint8_t pad0;
        uint32_t pad[5];         /**< Padding */
        uint32_t full_sequence;  /**< full sequence */
    } xcb_generic_error_t;

    typedef struct {
        unsigned int sequence;  /**< Sequence number */
    } xcb_void_cookie_t;
""")

# connection manipulation
ffi.cdef("""
    typedef ... xcb_connection_t;
    int xcb_connection_has_error(xcb_connection_t *c);
""")

C = ffi.verify("""
    #include <xcb/xcb.h>
    #include <xcb/xcbext.h>
""", libraries=['xcb'])

for c in constants:
    globals()[c] = getattr(C, c)
