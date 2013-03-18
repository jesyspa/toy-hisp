CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS     :=

all : haskell

haskell :
	runhaskell Setup.hs configure $(CABAL-CONFIGURE-FLAGS)
	runhaskell Setup.hs build $(CABAL-BUILD-FLAGS)
	ln -sf dist/build/main/main .

.PHONY : haskell
.PHONY : clean

clean :
	runhaskell Setup.hs clean
	rm -f main
