CC = ghc

all: main

main: src/Main.hs src/Mock.hs
	$(CC) -o interpreter src/Main.hs src/Mock.hs

clean:
	rm -f *.o *.hi interpreter
