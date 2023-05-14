all: interpreter

interpreter:
	cd src && make
	ln -fs src/Interpreter interpreter

test:
	./test.sh

# todo: remove in release
simple_test:
	./interpreter ../tests/simple

clean:
	cd src && make clean
	rm -f interpreter src/Interpreter src/TestGramatyka
