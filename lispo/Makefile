# target without .hs
T=lispo
A=-dynamic --make
# -XTemplateHaskell -ddump-splices -ddump-to-file
$T:	*.hs
	ghc $A $T.hs

.PHONY:	test
test:
	nl test.txt
	cat test.txt | ./$T | nl

.PHONY:	clean
clean:
	rm -f *.hi *.o
