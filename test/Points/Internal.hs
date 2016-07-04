{- |
   Module     : Points.KdTree
   Copyright  : Copyright (c) 2011 Issac Trotts
   License    : see LICENSE
                       
   Maintainer : Michael Litchard, Issac Trotts
   Stability  : experimental
   Portability: not portable
                                                                        
  This module provides provides Arbitrary instance and helper functions for
  Data/Trees/KdTree/Points/KdTree.hs. 

-}

module Points.Internal ( allSubtreesAreValid ) where

import Test.QuickCheck
import Data.Trees.KdTree.Points.Internal
instance Arbitrary Point3d where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Point3d x y z)

-- |isValid tells whether the K-D tree property holds for a given tree.
-- Specifically, it tests that all points in the left subtree lie to the left
-- of the plane, p is on the plane, and all points in the right subtree lie to
-- the right.
isValid :: Point p => KdTree p -> Bool
isValid KdEmpty = True
isValid (KdNode l p r axis) = leftIsGood && rightIsGood
    where x = coord axis p
          leftIsGood = all ((<= x) . coord axis) (toList l)
          rightIsGood = all ((>= x) . coord axis) (toList r)

-- |allSubtreesAreValid tells whether the K-D tree property holds for the given
-- tree and all subtrees.
allSubtreesAreValid :: Point p => KdTree p -> Bool
allSubtreesAreValid = all isValid . subtrees
