{- |
   Module     : Points.KdTree
   Copyright  : Copyright (c) 2016 Michael Litchard
   License    : MIT
            
   Maintainer : Michael Litchard, Issac Trotts
   Stability  : experimental
   Portability: not portable
                                        
   This module organizes the property tests for
   Data/Trees/KdTree/Points/KdTree.hs. 

-}

module Points.KdTree 
  ( propConstructionTests
  , propNearestNeighbor 
  , propRemove
  ) where
 
import Test.Hspec (Spec, describe,it)
import Test.QuickCheck (property)
import Points.KdTreeTest

propConstructionTests :: Spec
propConstructionTests = 
  describe "Construction Tests" $ do
    it "prop_constructionProducesValidTrees" $
       property prop_constructionProducesValidTrees
    it "prop_samePoints" $
       property prop_samePoints
     
propNearestNeighbor :: Spec
propNearestNeighbor =
  describe "Nearest Neighbor Tests" $ do
    it "prop_nearestNeighbor" $
      property prop_nearestNeighbor
    it "prop_nearNeighbors" $
      property prop_nearNeighbors  
    it "prop_pointsAreClosestToThemselves" $
      property prop_pointsAreClosestToThemselves
    it "prop_kNearestNeighborsMatchesBrute" $
      property prop_kNearestNeighborsMatchesBrute

propRemove :: Spec
propRemove =
  describe "Testing remove function properties" $ do
    it "prop_removeReallyRemovesPoints" $
      property prop_removeReallyRemovesPoints
    it "prop_removePreservesInvariant" $ 
      property prop_removePreservesInvariant
