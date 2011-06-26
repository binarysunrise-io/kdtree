{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Maybe
import qualified Data.List as L

import Test.QuickCheck
import Test.QuickCheck.All

import qualified Data.Trees.KdTree as Kd

prop_invariant :: [Kd.Point3d] -> Bool
prop_invariant points = Kd.invariant' . Kd.fromList $ points

prop_samePoints :: [Kd.Point3d] -> Bool
prop_samePoints points = L.sort points == (L.sort . Kd.toList . Kd.fromList $ points)

prop_nearestNeighbor :: [Kd.Point3d] -> Kd.Point3d -> Bool
prop_nearestNeighbor points probe =
    Kd.nearestNeighbor tree probe == bruteNearestNeighbor points probe
    where tree = Kd.fromList points

prop_pointsAreClosestToThemselves :: [Kd.Point3d] -> Bool
prop_pointsAreClosestToThemselves points =
    map Just points == map (Kd.nearestNeighbor tree) points
    where tree = Kd.fromList points

bruteNearestNeighbor :: [Kd.Point3d] -> Kd.Point3d -> Maybe Kd.Point3d
bruteNearestNeighbor [] _ = Nothing
bruteNearestNeighbor points probe =
    Just . head . L.sortBy (Kd.compareDistance probe) $ points

main = $quickCheckAll

