default: slownie

slownie: slownie.hs Slownie.hs
	ghc slownie.hs

clean:
	-rm -f *.o
	-rm -f *.hi
