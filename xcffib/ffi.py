from cffi import FFI

# TODO: don't require a c compiler at runtime :-)
# http://cffi.readthedocs.org/en/release-0.8/index.html?highlight=ffilibrary#distributing-modules-using-cffi

ffi = FFI()

CONSTANTS = [
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
ffi.cdef('\n'.join("#define %s ..." % c for c in CONSTANTS))

# types
ffi.cdef("""
    // xcb.h
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

    typedef struct xcb_auth_info_t {
        int   namelen;
        char *name;
        int   datalen;
        char *data;
    } xcb_auth_info_t;

    typedef ... xcb_connection_t;
    typedef ... xcb_extension_t;

    // xproto.h
    typedef struct xcb_query_extension_reply_t {
        uint8_t  response_type;
        uint8_t  pad0;
        uint16_t sequence;
        uint32_t length;
        uint8_t  present;
        uint8_t  major_opcode;
        uint8_t  first_event;
        uint8_t  first_error;
    } xcb_query_extension_reply_t;

    typedef ... xcb_setup_t;
""")

# connection manipulation, mostly generated with:
# grep -v '^[ \/\}#]' xcb.h | grep -v '^typedef' | grep -v '^extern'
ffi.cdef("""
    int xcb_flush(xcb_connection_t *c);
    uint32_t xcb_get_maximum_request_length(xcb_connection_t *c);
    void xcb_prefetch_maximum_request_length(xcb_connection_t *c);
    xcb_generic_event_t *xcb_wait_for_event(xcb_connection_t *c);
    xcb_generic_event_t *xcb_poll_for_event(xcb_connection_t *c);
    const xcb_query_extension_reply_t *xcb_get_extension_data(xcb_connection_t *c, xcb_extension_t *ext);
    const xcb_setup_t *xcb_get_setup(xcb_connection_t *c);
    int xcb_get_file_descriptor(xcb_connection_t *c);
    int xcb_connection_has_error(xcb_connection_t *c);
    xcb_connection_t *xcb_connect_to_fd(int fd, xcb_auth_info_t *auth_info);
    void xcb_disconnect(xcb_connection_t *c);
    int xcb_parse_display(const char *name, char **host, int *display, int *screen);
    xcb_connection_t *xcb_connect(const char *displayname, int *screenp);
    xcb_connection_t *xcb_connect_to_display_with_auth_info(const char *display, xcb_auth_info_t *auth, int *screen);
    uint32_t xcb_generate_id(xcb_connection_t *c);
""")

C = ffi.verify("""
    #include <xcb/xcb.h>
    #include <xcb/xcbext.h>
""", libraries=['xcb'])
