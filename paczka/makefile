CC = ghc

all: main

main:
	cd src && make
	cp src/interpreter interpreter

test:
	./test.sh

simple_test:
	./interpreter good/04-print

# todo: clean
clean:
	rm -f *.o *.hi interpreter
