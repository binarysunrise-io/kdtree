KdTreeTest: Data/Trees/KdTree.hs KdTreeTest.hs
	ghc --make $@

test: KdTreeTest
	./KdTreeTest

