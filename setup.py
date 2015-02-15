#!/usr/bin/env python
#
# Copyright 2014 Tycho Andersen
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

import os
import sys
import subprocess

from setuptools import setup, find_packages
from distutils.command.build import build

# Stolen from http://github.com/xattr/xattr, which is also MIT licensed.
class cffi_build(build):
    """This is a shameful hack to ensure that cffi is present when
    we specify ext_modules. We can't do this eagerly because
    setup_requires hasn't run yet.
    """
    def finalize_options(self):
        if not os.path.exists('./xcffib'):
            print("It looks like you need to generate the binding.")
            print("please run 'make xcffib' or 'make check'.")
            sys.exit(1)

        import xcffib
        self.distribution.ext_modules = [xcffib.ffi.verifier.get_extension()]
        build.finalize_options(self)

version = "0.2.0"
dependencies = ['six', 'cffi>=0.8.2']

setup(
    name="xcffib",
    version=version,
    description="A drop in replacement for xpyb, an XCB python binding",
    keywords="xcb xpyb cffi x11 x windows",
    license="Apache License 2.0",
    url="http://github.com/tych0/xcffib",
    author="Tycho Andersen",
    author_email="tycho@tycho.ws",
    install_requires=dependencies,
    setup_requires=dependencies,
    packages=['xcffib'],
    zip_safe=False,
    cmdclass={'build': cffi_build},
)
