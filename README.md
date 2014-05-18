# XCFFIB

`xcffib` is intended to be a (mostly) drop-in replacement for `xpyb`. `xpyb`
has an inactive upstream, several memory leaks, is python2 only and doesn't
have pypy support. `xcffib` is a binding which uses
[cffi][https://cffi.readthedocs.org/], which mitigates some of the issues
described above.

## Dependencies

Currently `xcb-types` doesn't run against `xcb-proto` 1.10; there is a hacked
branch available at [tych0/xcb-types][http://github.com/tych0/xcb-types] that
allows you to parse xcb-proto 1.10 mostly correctly. Other than that, you
should be able to install all the deps from hackage or pip.

## Differences

There are lots of differences right now between `xpyb` and `xcffib`, mostly
because `xcffib` isn't done yet :-)

## Why haskell?

Why is the binding generator written in haskell? Because haskell is awesome.
