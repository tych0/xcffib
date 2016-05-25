# Copyright 2014 Tycho Andersen
# Copyright 2014 Sean Vig
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

from cffi import FFI


CONSTANTS = [
    ("X_PROTOCOL", 11),
    ("X_PROTOCOL_REVISION", 0),
    ("X_TCP_PORT", 6000),

    ("XCB_NONE", 0),
    ("XCB_COPY_FROM_PARENT", 0),
    ("XCB_CURRENT_TIME", 0),
    ("XCB_NO_SYMBOL", 0),

    ("XCB_CONN_ERROR", 1),
    ("XCB_CONN_CLOSED_EXT_NOTSUPPORTED", 2),
    ("XCB_CONN_CLOSED_MEM_INSUFFICIENT", 3),
    ("XCB_CONN_CLOSED_REQ_LEN_EXCEED", 4),
    ("XCB_CONN_CLOSED_PARSE_ERR", 5),
    ("XCB_CONN_CLOSED_INVALID_SCREEN", 6),
    ("XCB_CONN_CLOSED_FDPASSING_FAILED", 7),

    ("XCB_REQUEST_CHECKED", 1 << 0)
]


# constants
CDEF = '\n'.join("#define %s %d" % (c, v) for c, v in CONSTANTS)

# types
CDEF += """
    // xcb.h
    typedef struct {
        uint8_t  response_type;  /**< Type of the response */
        uint8_t  pad0;           /**< Padding */
        uint16_t sequence;       /**< Sequence number */
        uint32_t length;         /**< Length of the response */
    } xcb_generic_reply_t;

    typedef struct {
        uint8_t  response_type;  /**< Type of the response */
        uint8_t  pad0;           /**< Padding */
        uint16_t sequence;       /**< Sequence number */
        uint32_t pad[7];         /**< Padding */
        uint32_t full_sequence;  /**< full sequence */
    } xcb_generic_event_t;

    typedef struct {
        uint8_t  response_type;  /**< Type of the response */
        uint8_t  error_code;     /**< Error code */
        uint16_t sequence;       /**< Sequence number */
        uint32_t resource_id;     /** < Resource ID for requests with side effects only */
        uint16_t minor_code;      /** < Minor opcode of the failed request */
        uint8_t  major_code;       /** < Major opcode of the failed request */
        uint8_t  pad0;
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

    // xproto.h
    typedef uint32_t xcb_colormap_t;
    typedef uint32_t xcb_drawable_t;
    typedef uint32_t xcb_pixmap_t;
    typedef uint32_t xcb_visualid_t;
    typedef uint32_t xcb_window_t;

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

    typedef struct xcb_setup_t {
        uint8_t       status; /**<  */
        uint8_t       pad0; /**<  */
        uint16_t      protocol_major_version; /**<  */
        uint16_t      protocol_minor_version; /**<  */
        uint16_t      length; /**<  */
        uint32_t      release_number; /**<  */
        uint32_t      resource_id_base; /**<  */
        uint32_t      resource_id_mask; /**<  */
        uint32_t      motion_buffer_size; /**<  */
        uint16_t      vendor_len; /**<  */
        uint16_t      maximum_request_length; /**<  */
        uint8_t       roots_len; /**<  */
        uint8_t       pixmap_formats_len; /**<  */
        uint8_t       image_byte_order; /**<  */
        uint8_t       bitmap_format_bit_order; /**<  */
        uint8_t       bitmap_format_scanline_unit; /**<  */
        uint8_t       bitmap_format_scanline_pad; /**<  */
        uint8_t       min_keycode; /**<  */
        uint8_t       max_keycode; /**<  */
        uint8_t       pad1[4]; /**<  */
    } xcb_setup_t;

    typedef struct xcb_visualtype_t {
        xcb_visualid_t visual_id; /**<  */
        uint8_t        _class; /**<  */
        uint8_t        bits_per_rgb_value; /**<  */
        uint16_t       colormap_entries; /**<  */
        uint32_t       red_mask; /**<  */
        uint32_t       green_mask; /**<  */
        uint32_t       blue_mask; /**<  */
        uint8_t        pad0[4]; /**<  */
    } xcb_visualtype_t;

    typedef struct xcb_screen_t {
        xcb_window_t   root; /**<  */
        xcb_colormap_t default_colormap; /**<  */
        uint32_t       white_pixel; /**<  */
        uint32_t       black_pixel; /**<  */
        uint32_t       current_input_masks; /**<  */
        uint16_t       width_in_pixels; /**<  */
        uint16_t       height_in_pixels; /**<  */
        uint16_t       width_in_millimeters; /**<  */
        uint16_t       height_in_millimeters; /**<  */
        uint16_t       min_installed_maps; /**<  */
        uint16_t       max_installed_maps; /**<  */
        xcb_visualid_t root_visual; /**<  */
        uint8_t        backing_stores; /**<  */
        uint8_t        save_unders; /**<  */
        uint8_t        root_depth; /**<  */
        uint8_t        allowed_depths_len; /**<  */
    } xcb_screen_t;

    typedef struct xcb_screen_iterator_t {
        xcb_screen_t *data; /**<  */
        int           rem; /**<  */
        int           index; /**<  */
    } xcb_screen_iterator_t;

    xcb_screen_iterator_t
    xcb_setup_roots_iterator (const xcb_setup_t *R  /**< */);

    void
    xcb_screen_next (xcb_screen_iterator_t *i  /**< */);

    // render.h
    typedef uint32_t xcb_render_pictformat_t;

    typedef struct xcb_render_directformat_t {
        uint16_t red_shift; /**<  */
        uint16_t red_mask; /**<  */
        uint16_t green_shift; /**<  */
        uint16_t green_mask; /**<  */
        uint16_t blue_shift; /**<  */
        uint16_t blue_mask; /**<  */
        uint16_t alpha_shift; /**<  */
        uint16_t alpha_mask; /**<  */
    } xcb_render_directformat_t;

    typedef struct xcb_render_pictforminfo_t {
        xcb_render_pictformat_t   id; /**<  */
        uint8_t                   type; /**<  */
        uint8_t                   depth; /**<  */
        uint8_t                   pad0[2]; /**<  */
        xcb_render_directformat_t direct; /**<  */
        xcb_colormap_t            colormap; /**<  */
    } xcb_render_pictforminfo_t;

    // xcbext.h
    typedef struct xcb_extension_t {
        const char *name;
        int global_id;
    } xcb_extension_t;

    typedef struct {
        size_t count;
        xcb_extension_t *ext;
        uint8_t opcode;
        uint8_t isvoid;
    } xcb_protocol_request_t;

    // sys/uio.h
    struct iovec
    {
      void *iov_base; /* BSD uses caddr_t (1003.1g requires void *) */
      size_t iov_len; /* Must be size_t (1003.1g) */
    };

    // need to manually free some things that XCB allocates
    void free(void *ptr);
"""

# connection manipulation, mostly generated with:
# grep -v '^[ \/\}#]' xcb.h | grep -v '^typedef' | grep -v '^extern'
CDEF += """
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
    xcb_generic_error_t *xcb_request_check(xcb_connection_t *c, xcb_void_cookie_t cookie);
"""

CDEF += """
    unsigned int xcb_send_request(xcb_connection_t *c, int flags, struct iovec *vector, const xcb_protocol_request_t *request);
    void *xcb_wait_for_reply(xcb_connection_t *c, unsigned int request, xcb_generic_error_t **e);
    int xcb_poll_for_reply(xcb_connection_t *c, unsigned int request, void **reply, xcb_generic_error_t **error);
    void xcb_discard_reply(xcb_connection_t *c, unsigned int sequence);
"""


ffi = FFI()
if hasattr(ffi, 'set_source'):  # PyPy < 2.6 compatibility hack
    ffi.set_source("xcffib._ffi", None, libraries=['xcb'])
    do_compile = True
else:
    do_compile = False
ffi.cdef(CDEF)


if __name__ == "__main__" and do_compile:
    ffi.compile()
