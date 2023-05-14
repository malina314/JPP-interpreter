CC = ghc

all: main

main:
	cd src && make
	ln -fs src/interpreter interpreter

test:
	./test.sh

simple_test:
	./interpreter ../tests/simple

# todo: clean
clean:
	rm -f *.o *.hi interpreter
