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
.PHONY : test

test : haskell lib/libdbghisp.a
	./main < main.hisp > main.cpp
	g++ -o m -Wall -Wextra -Werror -pedantic -std=c++1y -O0 -g3 -D_GLIBCXX_DEBUG main.cpp -Llib -ldbghisp
	valgrind ./m

clean :
	runhaskell Setup.hs clean
	rm -f main m main.cpp
	scons -c
