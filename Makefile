AUTOPEP8=autopep8 --in-place --aggressive --aggressive

XCBDIR?=$(shell pkg-config --variable=xcbincludedir xcb-proto)
ifneq ($(XCBDIR),$(shell pkg-config --variable=xcbincludedir xcb-proto))
	XCBVER=$(shell sed -e '1,/AC_INIT/d' $(XCBDIR)/../configure.ac | head -n 1 | tr -d ,[:blank:])
else
	XCBVER=$(shell pkg-config --modversion xcb-proto)
endif
NCPUS=$(shell grep -c processor /proc/cpuinfo)
PARALLEL=$(shell which parallel)
CABAL=cabal --config-file=./cabal.config
GEN=$(CABAL) new-run --minimize-conflict-set -j$(NCPUS) exe:xcffibgen --

# you should have xcb-proto installed to run this
xcffib: module/*.py xcffib.cabal $(shell find . -path ./test -prune -false -o -name \*.hs)
	$(GEN) --input $(XCBDIR) --output ./xcffib
	cp ./module/*py ./xcffib/
	touch ./xcffib/py.typed
	sed -i "s/__xcb_proto_version__ = .*/__xcb_proto_version__ = \"${XCBVER}\"/" xcffib/__init__.py

.PHONY: xcffib-fmt
xcffib-fmt: module/*.py
ifeq (${PARALLEL},)
	$(AUTOPEP8) ./xcffib/*.py
else
	find ./xcffib/*.py | parallel -j $(NCPUS) $(AUTOPEP8) '{}'
endif

dist-newstyle:
	$(CABAL) new-configure --enable-tests

.PHONY: gen
gen: dist-newstyle
	$(CABAL) new-build -j$(NCPUS)

.PHONY: clean
clean:
	-$(CABAL) new-clean
	-rm -rf xcffib
	-rm -rf module/*pyc module/__pycache__
	-rm -rf test/*pyc test/__pycache__
	-rm -rf build *egg* *deb .pybuild dist
	-rm -rf .pc cabal.project.local*

valgrind: xcffib
	valgrind --leak-check=full --show-leak-kinds=definite pytest-3 -v

newtests:
	$(GEN) --input ./test/generator/ --output ./test/generator/
	git diff test

# These are all split out so make -j3 check goes as fast as possible.
.PHONY: lint
lint:
	flake8 --config=./test/flake8.cfg ./module

.PHONY: htests
htests:
	$(CABAL) new-test -j$(NCPUS) --enable-tests

# The --builtin=CW is a hack to work around:
# https://lists.freedesktop.org/archives/xcb/2022-December/011427.html
# when that lands and all tested versions have it, we can
# drop this.
#
# In the meantime, we can work around this in the binding if someone really needs it.
ifeq ($(shell printf "$(XCBVER)\n1.16.0" | sort -V | grep -c 1.16.0), 1)
CW_HACK=--builtins=CW
endif

check: xcffib lint htests
	cabal check
	flake8 -j$(NCPUS) --ignore=E128,E231,E251,E301,E302,E305,E501,F401,E402,W503,E741,E999 xcffib/*.py $(CW_HACK)
	python3 -m compileall xcffib
	pytest-3 -v --durations=3 -n auto

# make release ver=0.99.99
release: xcffib
ifeq (${ver},)
	@echo "no version (ver=) specified, not releasing."
else ifneq ($(wildcard ./xcffib.egg-info*),)
	@echo "xcffib.egg-info exists, not releasing."
else
	sed -i "s/version = .*/version = \"${ver}\"/" setup.py
	sed -i "s/__version__ = .*/__version__ = \"${ver}\"/" xcffib/__init__.py
	sed -r -i -e "s/(^version = \s*)[\"0-9\.]*/\1\"${ver}\"/" setup.py
	sed -r -i -e "s/(^version:\s*)[0-9\.]*/\1${ver}/" xcffib.cabal
	echo "Release v${ver}" > /tmp/xcffib.releasemsg
	git commit -a -S -s --allow-empty-message -t /tmp/xcffib.releasemsg
	git tag v${ver}
	python3 setup.py sdist
	twine upload dist/xcffib-${ver}.tar.gz
	cabal new-sdist
	cabal upload --publish dist-newstyle/sdist/xcffib-${ver}.tar.gz
	@echo "remember to push the tag!!!"
endif
