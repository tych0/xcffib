import ctypes.util
import platform

# Attempt api mode, then precompiled abi mode, then import time abi
cffi_mode = "(unknown)"
try:
    # Note in ABI mode lib is already available, no dlopen() needed
    from ._xcffib import ffi, lib
    cffi_mode = "api"
except ImportError:
    try:
        # Note in ABI mode lib will be missing
        from ._xcffib import ffi
        cffi_mode = "abi"
    except ImportError:
        # This means that someone is trying to import the module *without*
        # having run ffi_build.py. We could "just" build it for them via:
        #    ffi = ffi_for_mode("abi")
        # but this is not an expected configuration, and it's hard to test, so
        # let's not support it for now.
        raise

# Fall back to non api mode, inline at load time
# this would only happen if the precompiled _xcffib was missing
if cffi_mode != "api":
    if platform.system() == "Darwin":
        soname = "libxcb.dylib"
    elif platform.system() == "Windows":
        soname = "libxcb.dll"
    else:
        soname = ctypes.util.find_library("xcb")
        if soname is None:
            soname = "libxcb.so"
    lib = ffi.dlopen(soname)  # noqa
