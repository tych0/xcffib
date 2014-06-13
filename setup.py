#!/usr/bin/env python

import os
import sys
import subprocess

from setuptools import setup, find_packages

if not os.path.exists('./xcffib'):
    print("It looks like you need to generate the binding.")
    print("please run 'make xcffib' or 'make check'.")
    sys.exit(1)

# version = subprocess.check_output(['git', 'describe', '--tags'])

import xcffib

setup(
    name="xcffib",
    version="prerelease",
    description="A drop in replacement for xpyb, an XCB python binding",
    keywords="xcb xpyb cffi x11 x windows",
    license="MIT",
    install_requires=['six', 'cffi>=0.8.2'],
    packages=['xcffib'],
    zip_safe=False,
    ext_modules=[xcffib.ffi.verifier.get_extension()],
)
