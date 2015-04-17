test:
	find . -name '*_Test.hs' | xargs -n1 runhaskell

quickcheck:
	quickCheck *.hs

