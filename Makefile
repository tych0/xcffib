GEN=./dist/build/xcffibgen/xcffibgen

.PHONY: $(GEN)
$(GEN):
	cabal build

.PHONY: clean
clean:
	cabal clean
	rm -rf xcffib/*pyc xcffib/__pycache__

# you should have xcb-proto installed to run this
check: $(GEN)
	cabal test
	$(GEN) --input /usr/share/xcb --output ./dist/gen/
	cp ./xcffib/*.py ./dist/gen/
	flake8 ./dist/gen/
