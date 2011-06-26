{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.List as L

import Test.QuickCheck
import Test.QuickCheck.All

import KdTree

-- |invariant tells whether the KD tree property holds for a given tree and
-- all its subtrees.
-- Specifically, it tests that all points in the left subtree lie to the left
-- of the plane, p is on the plane, and all points in the right subtree lie to
-- the right.
invariant :: Point p => KdTree p -> Bool
invariant KdEmpty = True
invariant (KdNode l p r axis) = leftIsGood && rightIsGood
    where x = coord axis p
	  leftIsGood = all ((<= x) . coord axis) (toList l)
	  rightIsGood = all ((>= x) . coord axis) (toList r)

invariant' :: Point p => KdTree p -> Bool
invariant' = all invariant . subtrees

instance Arbitrary Point3d where
    arbitrary = do
	x <- arbitrary
	y <- arbitrary
	z <- arbitrary
	return (Point3d x y z)

prop_invariant :: [Point3d] -> Bool
prop_invariant points = invariant' . fromList $ points

prop_samePoints :: [Point3d] -> Bool
prop_samePoints points = L.sort points == (L.sort . toList . fromList $ points)

prop_nearestNeighbor :: [Point3d] -> Point3d -> Bool
prop_nearestNeighbor points probe =
    nearestNeighbor tree probe == bruteNearestNeighbor points probe
    where tree = fromList points

bruteNearestNeighbor :: [Point3d] -> Point3d -> Maybe Point3d
bruteNearestNeighbor [] _ = Nothing
bruteNearestNeighbor points probe =
    Just . head . L.sortBy (compareDistance probe) $ points

main = $quickCheckAll

