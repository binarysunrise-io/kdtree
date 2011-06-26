{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.List as L

import Test.QuickCheck
import Test.QuickCheck.All

import Data.Trees.KdTree

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

