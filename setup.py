#!/usr/bin/env python

import os
import sys
import subprocess

from setuptools import setup, find_packages
from distutils.command.build import build

if not os.path.exists('./xcffib'):
    print("It looks like you need to generate the binding.")
    print("please run 'make xcffib' or 'make check'.")
    sys.exit(1)

# Stolen from http://github.com/xattr/xattr, which is also MIT licensed.
class cffi_build(build):
    """This is a shameful hack to ensure that cffi is present when
    we specify ext_modules. We can't do this eagerly because
    setup_requires hasn't run yet.
    """
    def finalize_options(self):
        import xcffib
        self.distribution.ext_modules = [xcffib.ffi.verifier.get_extension()]
        build.finalize_options(self)

version = "v0.1.4"
dependencies = ['six', 'cffi>=0.8.2']

setup(
    name="xcffib",
    version=version,
    description="A drop in replacement for xpyb, an XCB python binding",
    keywords="xcb xpyb cffi x11 x windows",
    license="MIT",
    url="http://github.com/tych0/xcffib",
    author="Tycho Andersen",
    author_email="tycho@tycho.ws",
    install_requires=dependencies,
    setup_requires=dependencies,
    packages=['xcffib'],
    zip_safe=False,
    cmdclass={'build': cffi_build},
)
