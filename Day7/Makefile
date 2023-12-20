GHC=ghc
FLAGS=-O

cards: main.hs Camelcards.hs
	$(GHC) $(FLAGS) -o $@ $?

clean:
	rm -vf *.o *.hi cards