CC = ghc

all: grammar main

grammar:
	cd src/BnfcParser && bnfc -m --functor grammar.cf && make

main: src/Main.hs src/Mock.hs
	$(CC) -isrc -v -o interpreter src/Main.hs src/Mock.hs

clean:
	rm -f *.o *.hi interpreter
