GEN=./dist/build/xcffibgen/xcffibgen

.PHONY: $(GEN)
$(GEN):
	cabal build

.PHONY: clean
clean:
	cabal clean
	rm -rf xcffib
	rm -rf module/*pyc module/__pycache__

xcffib: $(GEN)
	$(GEN) --input /usr/share/xcb --output ./xcffib
	cp ./module/*py ./xcffib/

newtests: $(GEN)
	$(GEN) --input ./tests/generator/ --output ./tests/generator/

# you should have xcb-proto installed to run this
check: xcffib
	cabal test
	flake8 ./xcffib
	xvfb-run nosetests -d
