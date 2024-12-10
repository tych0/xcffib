AUTOPEP8=autopep8 --in-place --aggressive --aggressive

XCBDIR?=$(shell pkg-config --variable=xcbincludedir xcb-proto)
ifneq ($(XCBDIR),$(shell pkg-config --variable=xcbincludedir xcb-proto))
	XCBVER=$(shell sed -e '1,/AC_INIT/d' $(XCBDIR)/../configure.ac | head -n 1 | tr -d ,[:blank:])
else
	XCBVER=$(shell pkg-config --modversion xcb-proto)
endif
NCPUS=$(shell grep -c processor /proc/cpuinfo)
PARALLEL=$(shell which parallel)
CABAL=flock xcffib.cabal cabal --config-file=./cabal.config
GEN=$(CABAL) new-run --minimize-conflict-set -j$(NCPUS) exe:xcffibgen --
VENV=xcffib_venv
PYTHON=$(VENV)/bin/python3
FLAKE=$(VENV)/bin/flake8

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
	-rm -rf xcffib xcffib_venv
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
lint: $(VENV)
	$(FLAKE) --config=./test/flake8.cfg ./module

.PHONY: htests
htests:
	$(CABAL) new-test -j$(NCPUS) --enable-tests

$(VENV): requirements.txt
	# the python in $PATH in CI is the python from the matrix, so it is the
	# "right" python to start with
	python3 -m venv $(VENV)
	$(PYTHON) -m pip install -r requirements.txt

check: xcffib htests $(VENV) lint
	cabal check
	$(PYTHON) -m compileall xcffib
	$(PYTHON) -m pytest -v --durations=3 -n $(NCPUS)

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
