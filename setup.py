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
from distutils.command.build import build
from distutils.command.install import install


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

# PyPy < 2.6 hack
if '_cffi_backend' in sys.builtin_module_names:
    import _cffi_backend
    requires_cffi = "cffi==" + _cffi_backend.__version__
else:
    requires_cffi = "cffi>=1.0.0"

version = "0.2.3"
dependencies = ['six', requires_cffi]

# PyPy < 2.6 hack
if requires_cffi.startswith("cffi==0."):
    # Have to use old methods to ensure cffi is installed and xcffib is generated
    class cffi_build(build):
        def finalize_options(self):
            if not os.path.exists('./xcffib'):
                print("It looks like you need to generate the binding.")
                print("please run 'make xcffib' or 'make check'.")
                sys.exit(1)

            from xcffib.ffi_build import ffi

            self.distribution.ext_modules = [ffi.verifier.get_extension()]
            build.finalize_options(self)

    class cffi_install(install):
        def finalize_options(self):
            if not os.path.exists('./xcffib'):
                print("It looks like you need to generate the binding.")
                print("please run 'make xcffib' or 'make check'.")
                sys.exit(1)

            from xcffib.ffi_build import ffi

            self.distribution.ext_modules = [ffi.verifier.get_extension()]
            install.finalize_options(self)

    cffi_args = dict(
        ext_package="xcffib",
        cmdclass={
            'build': cffi_build,
            'install': cffi_install
        }
    )
else:
    cffi_args = dict(
        cffi_modules=["xcffib/ffi_build.py:ffi"],
        cmdclass={
            'build': binding_build,
            'install': binding_install
        }
    )

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
    **cffi_args
)
