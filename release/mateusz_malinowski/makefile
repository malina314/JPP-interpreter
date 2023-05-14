all: interpreter

interpreter:
	cd src && make
	ln -fs src/Interpreter interpreter

test:
	./test.sh

clean:
	cd src && make clean
	rm -f interpreter src/Interpreter src/TestGramatyka
