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

from setuptools import setup
from setuptools.command.install import install
from distutils.command.build import build


class binding_build(build):
    """This is a check to ensure that the bindings have been generated, and
    print a helpful message if they have not been generated yet.  We only need
    to check this when we are actually building or installing.
    """
    def finalize_options(self):
        if not os.path.exists('./xcffib'):
            print("It looks like you need to generate the binding.")
            print("please run 'make xcffib' or 'make check'.")
            sys.exit(1)
        build.finalize_options(self)


class binding_install(install):
    def finalize_options(self):
        if not os.path.exists('./xcffib'):
            print("It looks like you need to generate the binding.")
            print("please run 'make xcffib' or 'make check'.")
            sys.exit(1)
        install.finalize_options(self)

version = "1.6.1"
dependencies = ["cffi>=1.1.0; python_implementation != 'PyPy'"]

setup(
    name="xcffib",
    version=version,
    description="xcffib is the XCB binding for python",
    keywords="xcb cffi x11 x windows",
    license="Apache License 2.0",
    url="http://github.com/tych0/xcffib",
    author="Tycho Andersen",
    author_email="tycho@tycho.pizza",
    install_requires=dependencies,
    setup_requires=dependencies,
    packages=['xcffib'],
    package_data={'xcffib': ['py.typed']},
    zip_safe=False,
    cmdclass={
        'build': binding_build,
        'install': binding_install
    },
    classifiers=[
        'Development Status :: 5 - Production/Stable',
        'License :: OSI Approved :: Apache Software License',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: Implementation :: CPython',
        'Programming Language :: Python :: Implementation :: PyPy',
        'Topic :: Software Development :: Libraries'
    ],
)
