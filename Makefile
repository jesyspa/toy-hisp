CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS     :=

all : haskell lib/libdbghisp.a

lib/libdbghisp.a : $(wildcard runtime/*.cpp)
	scons

lib/libhisp.a : $(wildcard runtime/*.cpp)
	scons release

haskell :
	runhaskell Setup.hs configure $(CABAL-CONFIGURE-FLAGS)
	runhaskell Setup.hs build $(CABAL-BUILD-FLAGS)
	ln -sf dist/build/main/main .

.PHONY : haskell
.PHONY : clean

clean :
	runhaskell Setup.hs clean
	rm -f main
	scons -c
