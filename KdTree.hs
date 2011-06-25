{-# LANGUAGE TemplateHaskell #-}

-- Based on
-- http://en.wikipedia.org/wiki/K-d_tree
-- Translated by Issac Trotts

import Data.List

import Test.QuickCheck
import Test.QuickCheck.All

class Point p where
      dimension :: p -> Int

      -- Get the k'th coordinate, starting from 0
      coord :: Int -> p -> Double

data Point3d = Point3d { p3x :: Double, p3y :: Double, p3z :: Double }
    deriving (Eq, Ord, Show)

instance Point Point3d where
    dimension _ = 3

    coord 0 p = p3x p
    coord 1 p = p3y p
    coord 2 p = p3z p


data KdTree point = KdNode { kdLeft :: KdTree point,
			     kdPoint :: point,
                             kdRight :: KdTree point }
                  | KdEmpty
     deriving (Eq, Ord, Show)

fromList :: Point p => [p] -> KdTree p
fromList points = fromListWithDepth points 0

-- Select axis based on depth so that axis cycles through all valid values
fromListWithDepth :: Point p => [p] -> Int -> KdTree p
fromListWithDepth [] _ = KdEmpty
fromListWithDepth points depth = node
    where   axis = axisFromDepth (head points) depth

	    -- Sort point list and choose median as pivot element
	    sortedPoints =
		sortBy (\a b -> coord axis a `compare` coord axis b) points
	    medianIndex = length sortedPoints `div` 2
	
	    -- Create node and construct subtrees
	    node = KdNode { kdLeft = fromListWithDepth (take medianIndex sortedPoints) (depth+1),
			    kdPoint = sortedPoints !! medianIndex,
			    kdRight = fromListWithDepth (drop (medianIndex+1) sortedPoints) (depth+1) }

axisFromDepth :: Point p => p -> Int -> Int
axisFromDepth p depth = depth `mod` k
    where k = dimension p

toList :: KdTree p -> [p]
toList KdEmpty = []
toList (KdNode l p r) = toList l ++ [p] ++ toList r

-- |invariant tells whether the KD tree property holds for a given tree.
-- Specifically, it tests that all points in the left subtree lie to the left
-- of the plane, p is on the plane, and all points in the right subtree lie to
-- the right.
invariant :: Point p => KdTree p -> Bool
invariant node = invariantWithDepth node 0
    where
	invariantWithDepth KdEmpty _ = True
	invariantWithDepth (KdNode l p r) depth =
	    leftIsGood && rightIsGood && leftRecurse && rightRecurse
	    where
		axis = axisFromDepth p depth
		x = coord axis p
		leftIsGood = all ((<= x) . coord axis) (toList l)
		rightIsGood = all ((>= x) . coord axis) (toList r)
		leftRecurse = invariantWithDepth l (depth + 1)
		rightRecurse = invariantWithDepth r (depth + 1)

-- Testing -----

instance Arbitrary Point3d where
    arbitrary = do
	x <- arbitrary
	y <- arbitrary
	z <- arbitrary
	return (Point3d x y z)

prop_invariant :: [Point3d] -> Bool
prop_invariant points = invariant . fromList $ points

prop_samePoints :: [Point3d] -> Bool
prop_samePoints points = sort points == (sort . toList . fromList $ points)

main = $quickCheckAll

