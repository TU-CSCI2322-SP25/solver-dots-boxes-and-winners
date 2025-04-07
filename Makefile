build:
	ghc --make -O -o DotsBoxes Main.hs

prof:
	ghc --make -prof -o DotsBoxes Main.hs

all: build test

# Cleaning commands:
clean:
	rm -f DotsBoxes
	rm -f *.hi
	rm -f *.o
