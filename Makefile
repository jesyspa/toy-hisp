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
.PHONY : test
.PHONY : clean
.PHONY : clean_tests

test : haskell runtime
	./run_tests.sh examples

clean : clean_tests
	cabal clean
	rm -rf build
	rm -f main hisp *.hic
	rm -rf output

clean_tests :
	rm -rf test-run-*
