{-# LANGUAGE FlexibleContexts #-}
{- |
   Module     : Regions.Internal
   Copyright  : Copyright (c) 2016 Michael Litchard
   License    : see LICENSE
                                  
   Maintainer : Michael Litchard
   Stability  : experimental
   Portability: not portable
                                                                                                                  
   This module provides provides  helper functions for
   Data/Trees/KdTree/Points/KdTree.hs. 

-}
module Regions.Internal (isValidK3) where
import Test.QuickCheck
import qualified Data.BoundingBox.Range as R
import Data.Vector.Fancy
import Data.Vector.V3
import Data.Vector.Class
import Data.BoundingBox hiding (isect)
import Data.BoundingBox.B3
import Data.Trees.KdTree.Regions.Internal (Branch (..))
import Data.Trees.KdTree.Regions.KThree.KThreeTree

isValidK3 :: Maybe Branch -> Axes BBox3 -> Scalar -> KdTree BBox3 a -> Bool
isValidK3 _ _ _ (KdLeaf Nothing)        = True
isValidK3 Nothing _ _ (KdLeaf (Just _)) = True -- degenerate Tree
isValidK3 (Just branch) axisb3 attrib (KdLeaf (Just (Leaf _ leaf))) = 
  let itest = case branch of
                BLeft  -> inequalityTest R.max_point (attrib >) range_f
                BRight -> inequalityTest R.min_point (attrib <) range_f
  in all (itest . fst) leaf
  where
    range_f = case axisb3 of
      (X AxisX) -> rangeX
      (Y AxisY) -> rangeY
      (Z AxisZ) -> rangeZ
  

isValidK3 _ _ _ (KdNode l node_bbox (axisb3, attrib) overlaps r) =
  overlapsAreGood && leftsAreGood && rightsAreGood
  where
    overlapsAreGood = all (intersects node_bbox) overlaps
    intersects node_bbox (test_box,_) = case (isect node_bbox test_box) of
      Just _  -> True
      Nothing -> False

    leftsAreGood    = isValidK3 (Just BLeft) axisb3 attrib l
    rightsAreGood   = isValidK3 (Just BRight) axisb3 attrib r
      


inequalityTest :: (R.Range -> Scalar) ->
                  (Scalar-> Bool)     ->
                  (BBox3 -> R.Range)   ->
                  BBox3               ->
                  Bool
inequalityTest find_endpoint inequality_test range_f bbox' =
  inequality_test endpoint
  where endpoint = find_endpoint (range_f bbox')

allSubtreesAreValid :: KdTree BBox3 a -> Bool
allSubtreesAreValid (KdLeaf _) = True -- Degenerate cases
allSubtreesAreValid k = all (subtreeIsValid) $ subtrees k
  where
    subtreeIsValid (KdNode l node_bbox (axisb3, attrib) overlaps r) =
      overlapsAreGood && leftsAreGood && rightsAreGood
      where
        overlapsAreGood = all (intersects node_bbox) overlaps
        intersects node_bbox (test_box,_) = case (isect node_bbox test_box) of
          Just _  -> True
          Nothing -> False
        leftsAreGood    = isValidK3 (Just BLeft) axisb3 attrib l
        rightsAreGood   = isValidK3 (Just BRight) axisb3 attrib r
  
  
subtrees :: KdTree BBox3 a -> [KdTree BBox3 a]
subtrees k@(KdLeaf _) = [k]
subtrees k@(KdNode l _ _ _ r) = subtrees l ++ [k] ++ subtrees r    
