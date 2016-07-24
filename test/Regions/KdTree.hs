{- |
   Module     : Regions.KdTree
   Copyright  : Copyright (c) 2016 Michael Litchard
   License    : MIT
            
   Maintainer : Michael Litchard, Issac Trotts
   Stability  : experimental
   Portability: not portable
                                        
   This module organizes the property tests for
   Data/Trees/KdTree/Points/KdTree.hs.

-}

module Regions.KdTree
 ( propConstructionRegionTests) where

import Test.Hspec (Spec, describe,it)
import Test.QuickCheck (property)
import Regions.KdTreeTests

propConstructionRegionTests :: Spec
propConstructionRegionTests =
  describe "Region Construction Tests" $ do
    it "prop_constructionProducesValidTrees" $
       property prop_constructionProducesValidTrees
