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
	runhaskell Setup.hs configure $(CABAL-CONFIGURE-FLAGS)
	runhaskell Setup.hs build $(CABAL-BUILD-FLAGS)
	ln -sf dist/build/main/main .

dotfiles := $(wildcard output/*.dot)

output/%.png : output/%.dot
	dot -Tpng $< > $@

pictures : ${dotfiles:.dot=.png}

.PHONY : haskell
.PHONY : runtime
.PHONY : clean

clean :
	runhaskell Setup.hs clean
	rm -rf build
	rm -f main hisp *.hic
	rm -rf output
