XCBDIR?=$(shell pkg-config --variable=xcbincludedir xcb-proto)
ifneq ($(XCBDIR),$(shell pkg-config --variable=xcbincludedir xcb-proto))
	XCBVER=$(shell sed -e '1,/AC_INIT/d' $(XCBDIR)/../configure.ac | head -n 1 | tr -d ,[:blank:])
else
	XCBVER=$(shell pkg-config --modversion xcb-proto)
endif
NCPUS=$(shell grep -c processor /proc/cpuinfo)
CABAL=flock xcffib.cabal cabal
GEN=$(CABAL) new-run --minimize-conflict-set -j$(NCPUS) exe:xcffibgen --
VENV=xcffib_venv
PYTHON=$(VENV)/bin/python3

GENERATED_TESTS_DIR=./test/generator/

# you should have xcb-proto installed to run this
xcffib: module/*.py xcffib.cabal $(shell find . -path ./test -prune -false -o -name \*.hs)
	$(GEN) --input $(XCBDIR) --output ./xcffib
	ruff format ./xcffib
	cp ./module/*py ./xcffib/
	touch ./xcffib/py.typed
	sed -i "s/__xcb_proto_version__ = .*/__xcb_proto_version__ = \"${XCBVER}\"/" xcffib/__init__.py

dist-newstyle:
	$(CABAL) new-configure --enable-tests

.PHONY: gen
gen: dist-newstyle
	$(CABAL) new-build -j$(NCPUS)

.PHONY: clean
clean:
	-$(CABAL) new-clean
	-rm -rf xcffib xcffib_venv
	-rm -rf module/*pyc module/__pycache__
	-rm -rf test/*pyc test/__pycache__
	-rm -rf build *egg* *deb .pybuild dist
	-rm -rf .pc cabal.project.local*

valgrind: xcffib
	valgrind --leak-check=full --show-leak-kinds=definite pytest-3 -v

newtests:
	$(GEN) --input $(GENERATED_TESTS_DIR) --output $(GENERATED_TESTS_DIR)
	git diff test

# These are all split out so make -j3 check goes as fast as possible.
.PHONY: lint
lint: $(VENV)
	ruff check --exclude $(GENERATED_TESTS_DIR) --exclude ./proto
	$(CABAL) check
	$(PYTHON) -m compileall xcffib

.PHONY: htests
htests:
	$(CABAL) new-test -j$(NCPUS) --enable-tests

$(VENV): pyproject.toml
	# the python in $PATH in CI is the python from the matrix, so it is the
	# "right" python to start with
	python3 -m venv $(VENV)
	$(PYTHON) -m pip install .[dev]

check: xcffib htests $(VENV) lint
	$(CABAL) check
	$(PYTHON) -m compileall xcffib
	$(PYTHON) -m pytest -v --durations=3 -n auto

# make release ver=0.99.99
release: xcffib
ifeq (${ver},)
	@echo "no version (ver=) specified, not releasing."
else ifneq ($(wildcard ./xcffib.egg-info*),)
	@echo "xcffib.egg-info exists, not releasing."
else
	sed -i "s/__version__ = .*/__version__ = \"${ver}\"/" xcffib/__init__.py
	sed -r -i -e "s/(^version = \s*)[\"0-9\.]*/\1\"${ver}\"/" pyproject.toml
	sed -r -i -e "s/(^version:\s*)[0-9\.]*/\1${ver}/" xcffib.cabal
	echo "Release v${ver}" > /tmp/xcffib.releasemsg
	git commit -a -S -s --allow-empty-message -t /tmp/xcffib.releasemsg
	git tag v${ver}
	python3 -m build --sdist
	twine upload dist/xcffib-${ver}.tar.gz
	cabal new-sdist
	cabal upload --publish dist-newstyle/sdist/xcffib-${ver}.tar.gz
	@echo "remember to push the tag!!!"
endif
