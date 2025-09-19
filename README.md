# xcffib [![Build Status](https://github.com/tych0/xcffib/actions/workflows/ci.yaml/badge.svg?branch=master)](https://github.com/tych0/xcffib/actions)

`xcffib` is the XCB binding for Python.

## Installation

For most end users of software that depends on xcffib or developers writing
code against xcffib, you can use the version of xcffib on pypi. To install it,
you'll need libxcb's headers and libxcb-render's headers (these are available
via `sudo apt-get install libxcb-render0-dev` on Ubuntu). Once you have the C
headers installed, you can just `pip install xcffib`.

If you're interested in doing development, read on...

## Development dependencies

You should be able to install all the language deps from hackage or pip.
[.github/workflows/ci.yaml](https://github.com/tych0/xcffib/blob/master/.github/workflows/ci.yaml)
has an example of how to install the dependencies on Ubuntu flavors.

## Hacking

See the [Makefile](https://github.com/tych0/xcffib/blob/master/Makefile) for
examples on how to run the tests. Your contribution should at pass `make check`
before it can be merged. The `newtests` make target can be used to regenerate
expected haskell test data if the tests are failing because you made a change
to the generated python code.

### Hacking on new xcbproto versions

Sometimes (more often recently), xcbproto makes some updates that we need to
do some work for. These often require some updates to `xcb-types` as well.
First, hack your changes into `xcb-types` and `cabal install` them, then git
clone the version of xcbproto you want to somewhere, e.g. `~/packages`:

    ~/packages $ git clone https://gitlab.freedesktop.org/xorg/proto/xcbproto.git

Finally, you can build/test xcffib against this custom version of
`xcb-{proto|types}` with:

    make XCBDIR=~/packages/xcbproto/src check

### Hacking on new xcb-types versions

To go along with new xcbproto elements, sometimes you need to hack on newer
versions of xcb-types. Newer cabals require you to do something like:

    echo packages: ../xcb-types/xcb-types.cabal ./xcffib.cabal > cabal.project

In order to find locally modified versions of xcb-types.
