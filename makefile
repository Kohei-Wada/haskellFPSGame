target = Main


all : $(target)


$(target) : Main.hs
	stack ghc $<



test : all
	./Main


clean : 
	rm -f $(target) *.o *.hi
