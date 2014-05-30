CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS     :=

all : haskell runtime

build/Makefile :
	mkdir -p build
	cd build; cmake -DCMAKE_BUILD_TYPE=Debug ..

runtime : build/Makefile
	$(MAKE) -C build
	ln -sf build/runtime/hisp .

haskell :
	cabal configure $(CABAL-CONFIGURE-FLAGS)
	cabal build $(CABAL-BUILD-FLAGS)
	ln -sf dist/build/main/main .

dotfiles := $(wildcard output/*.dot)

output/%.png : output/%.dot
	dot -Tpng $< > $@

pictures : ${dotfiles:.dot=.png}

.PHONY : haskell
.PHONY : runtime
.PHONY : clean
.PHONY : test

test : haskell runtime
	./run_tests.sh examples

clean :
	cabal clean
	rm -rf build
	rm -f main hisp *.hic
	rm -rf output
	rm -rf test-run-*
