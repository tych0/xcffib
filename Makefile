GEN=./dist/build/xcffibgen/xcffibgen

$(GEN): generator
	cabal build

.PHONY: clean
clean:
	cabal clean
	rm -rf xcffib/*pyc xcffib/__pycache__

# you should have xcb-proto installed to run this
check: $(GEN)
	cabal build
	$(GEN) --input /usr/share/xcb --output ./dist/gen/
	cp ./xcffib/__init__.py ./dist/gen/
