CC = ghc

all: main

main:
	cd src && make
	cp src/interpreter interpreter

test:
	./interpreter "../declaration/przyklady/03-zmienne,przypisanie"

# todo: clean
clean:
	rm -f *.o *.hi interpreter
