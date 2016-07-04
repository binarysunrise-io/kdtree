{- |
     Module     : tests/Main
     Copyright  : Copyright (c) 2016 Michael Litchard
     License    : MIT
                       
     Maintainer : Michael Litchard, Issac Trotts
     Stability  : experimental
     Portability: not portable
                                                     
     This module is the top-level test driver for all tests.

-}

import Test.Hspec (hspec)
import Points.KdTree

main = do
  hspec propConstructionTests
  hspec propNearestNeighbor
  hspec propRemove

