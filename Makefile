GEN=./dist/build/xcffibgen/xcffibgen

# you should have xcb-proto installed to run this
xcffib: $(GEN) module/*.py
	$(GEN) --input /usr/share/xcb --output ./xcffib
	cp ./module/*py ./xcffib/

dist:
	cabal configure

.PHONY: $(GEN)
$(GEN): dist
	cabal build

.PHONY: clean
clean:
	-cabal clean
	-rm -rf xcffib
	-rm -rf module/*pyc module/__pycache__
	-rm -rf tests/*pyc tests/__pycache__
	-rm -rf build *egg*

# A target for just running nosetests. Travis will run 'check', which does
# everything. (Additionally, travis uses separate environments where nosetests
# points to The Right Thing for each, so we don't need to do nosetests3.)
pycheck: xcffib
	nosetests -d
	nosetests3 -d

valgrind: xcffib
	valgrind --leak-check=full --show-leak-kinds=definite nosetests -d

newtests: $(GEN)
	$(GEN) --input ./tests/generator/ --output ./tests/generator/
	git diff tests

check: xcffib
	cabal test
	flake8 --config=./tests/flake8.cfg ./xcffib
	nosetests -d
