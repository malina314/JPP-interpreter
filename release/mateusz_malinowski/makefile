all: interpreter

interpreter:
	cd src && make
	ln -fs src/interpreter interpreter

test:
	./test.sh

clean:
	cd src && make clean
	rm -f interpreter src/interpreter src/TestGramatyka
