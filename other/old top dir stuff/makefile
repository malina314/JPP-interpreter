CC = ghc

all: grammar main

grammar:
	cd src && bnfc -m --functor grammar.cf && make

main: src/Main.hs src/Mock.hs
	# $(CC) -isrc -o interpreter src/Main.hs src/Mock.hs src/AbsGrammar.hs src/LexGrammar.hs src/ParGrammar.hs src/PrintGrammar.hs
	$(CC) -o interpreter src/Main.hs src/Mock.hs

clean:
	rm -f *.o *.hi interpreter
