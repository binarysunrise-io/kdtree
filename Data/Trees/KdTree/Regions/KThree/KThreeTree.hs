{- |
   Module     : Data.Trees.KdTree.Regions.3D.3DTree
   Copyright  : Copyright (c) 2016, Michael Litchard
   License    : BSD3
                       
   Maintainer : Michael Litchard
   Stability  : experimental
   Portability: not portable
                                                         
   This module implements an adaptation of kd-tree for regions.
   http://en.wikipedia.org/wiki/K-d_tree 

-}

{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances, InstanceSigs, ViewPatterns, DeriveFunctor #-}

module Data.Trees.KdTree.Regions.KThree.KThreeTree where

import Data.Maybe
import Data.Bool
import Data.Vector.V3
import Data.Vector.Class
import qualified Data.List as L
import Data.Vector.Fancy 
import qualified Data.BoundingBox.Range as R
import Data.BoundingBox
import Data.Ord
import Data.Semigroup
import Control.Monad
import Data.Trees.KdTree.Regions.Internal
-- | KdTree adapted to regions, based on
--   Foundations of Multidimensional and Metric Data Structures by Hanan Samet
--

leafSize :: Int
leafSize = 18

-- | 3-D Tree. Needs own module
instance KdTreeRegional BBox3 Vector3 where
--  type Payload Int = Int

  data Axes Vector3 = X AxisX | Y AxisY | Z AxisZ deriving Show
  data KdTree BBox3 a
    = KdNode {
        kdLeft     :: KdTree BBox3 a 
      , kdSplit    :: (Axes Vector3,Scalar)
      , overlapped :: [(BBox3, a)] -- can't get Region right
      , kdRight    :: KdTree BBox3 a 
      }
    | KdLeaf (Maybe [(BBox3, a)])
    deriving Show
--  type Region BBox3 a = [(BBox3, a)]
  data Collisions BBox3 a = Collisions [(BBox3,a)] deriving (Functor,Show)

  evalBox :: BBox3 -> Axes Vector3 -> Scalar -> Bool
  evalBox bbox axis scalar = case axis of
    (X AxisX) -> scalar > R.max_point (axis_range AxisX bbox)
    (Y AxisY) -> scalar > R.max_point (axis_range AxisY bbox)
    (Z AxisZ) -> scalar > R.max_point (axis_range AxisY bbox)
     

  nearestNeighbor :: KdTree BBox3 a -> 
                     BBox3          -> 
                     Either (Collisions BBox3 a) (Maybe [(BBox3, a)])
  nearestNeighbor (KdLeaf Nothing) _ = Right Nothing

  nearestNeighbor (KdLeaf (Just leaf)) bbox =
    isContained 
    bool Nothing (Just leaf) $ bbox `elem` (map fst leaf)

  -- | nearestNeighbor third case returns all possible candidates
  --   in the event there are no leaves connected to Node
  nearestNeighbor
    (KdNode (KdLeaf Nothing) _ overlapped (KdLeaf Nothing)) bbox =
      bool (Right Nothing) (Right $ Just overlapped) $ 
      bbox `elem` (map fst overlapped)

  nearestNeighbor (KdNode left (axis,scalar) overlapped right) bbox = undefined
{-    case (findCandidates) of
      []         -> Right Nothing
      candiates  -> findNearest bbox
    where 
      findNearest :: BBox3 -> (Maybe (Region a)) -> (Maybe (Region a))
      findNearest bbox regions =
        
      findCollisions = catMaybes $ join $ fmap (mayCollision box) candidates

      mayCollision :: BBox3 -> Maybe (Region a) -> Maybe (Region a)
      mayCollision _ Nothing = Nothing
      mayCollision bbox (Just regions) = 
        map (mayCollision' bbox) regions
        where
          mayCollision' bbox1 bbv@(bbox2,a) =
            case (isect bbox1 bbox2) of
              Nothing -> Nothing
              Just _  -> Just bbv 
           
      candidates = case (isOverlapping) of
        True  -> overlapped <> traverseBoth
        False -> overlapped <> traverseOne

      traverseOne =
        bool (nearestNeighbor left bbox) (nearestNeighbor right bbox) evalBox
 
      evalBox = case axis of
        (X AxisX) -> scalar > max_point (axis_range AxisX bbox) 
        (Y AxisY) -> scalar > max_point (axis_range AxisY bbox)
        (Z AxisZ) -> scalar > max_point (axis_range AxisY bbox)

      traverseBoth = nearestNeighbor left bbox <> nearestNeighbor right bbox
      isOverlapping = 
        fromMaybe False $
        bbox `elem` (fmap fst overlapped')
-}    
    
    
  toBBox :: [(Vector3,BBoxOffset,a)] ->
            [(BBox3,a)]
  toBBox [] = []
  toBBox xs = L.map toBBox' xs
    where
      toBBox' ((Vector3 x y z),offset,a) = (bbox3,a)
        where
          bbox3 = bound_corners nwu sed
          nwu = Vector3 (x - offset) (y + offset) (z + offset)
          sed = Vector3 (x + offset) (y - offset) (z - offset)
           
  fromList :: [(BBox3,a)] -> KdTree BBox3 a 
  fromList [] = KdLeaf Nothing
  fromList aList = fromSubList aList (X AxisX)

  fromSubList :: [(BBox3,a)] -> Axes Vector3 -> KdTree BBox3 a
  fromSubList [] _ = KdLeaf Nothing
  fromSubList blist@(length -> l) _ | l <= leafSize =
    KdLeaf (Just blist)
    
  fromSubList bList axis = node
    where
      node =
        KdNode {
          kdLeft = fromSubList front (nextAxis axis)
        , kdSplit = (axis, attrib_value axis split)
        , overlapped = overlaps axis split sortedBoxes
        , kdRight = fromSubList back (nextAxis axis)
        } 
      -- | helper functions prepping [(BBox3,Int)] for KdNode
      front = take medianIndex removedSplit
      back  = drop medianIndex removedSplit
      removedSplit = (map snd sortedBoxes)
      medianIndex = length sortedBoxes `div` 2
      split = fst (sortedBoxes !! medianIndex)

      nextAxis (X AxisX) = Y AxisY
      nextAxis (Y AxisY) = Z AxisZ
      nextAxis (Z AxisZ) = X AxisX

      -- | compute and map midpoint of each (BBox3,Int)
      --   for the sort
--    midpoints :: [(Vector3,(BBox3,a))]
      midpoints = map midpoint bList

      midpoint :: (BBox3,a) -> (Vector3,(BBox3,a))
      midpoint bp@(b,_) = (midpoint',bp)
        where
          midpoint' = Vector3 minx miny minz
          (Vector3 minx miny minz) = min_point b
          (Vector3 maxx maxy maxz) = max_point b
          midx = (maxx + minx) / 2
          midy = (maxy + miny) / 2
          midz = (maxz + minz) /2 

      -- | Sorts based on splitting axis
--    sortedBoxes :: [(Vector3,(BBox3,a))]
      sortedBoxes = 
        L.sortBy sort_by_attrib midpoints
  
      sort_by_attrib :: (Vector3,(BBox3,a)) ->
                        (Vector3,(BBox3,a)) -> 
                        Ordering
      sort_by_attrib p q =
        (attrib_value axis (fst p)) `compare` (attrib_value axis (fst q)) 
      attrib_value (X AxisX) vect = get_coord AxisX vect
      attrib_value (Y AxisY) vect = get_coord AxisY vect
      attrib_value (Z AxisZ) vect = get_coord AxisZ vect

      -- | Computes BBoxs overlapping split
      overlaps :: Axes Vector3          -> 
                  Vector3               -> 
                  [(Vector3,(BBox3,a))] -> 
                  [(BBox3, a)] 
      overlaps axis split sortedBoxes =
        mapMaybe (overlap axis split) sortedBoxes

      overlap :: Axes Vector3 -> 
                 Vector3 -> 
                 (Vector3,(BBox3,a)) -> 
                 Maybe (BBox3, a)
      overlap axis (Vector3 x y z) (_,(bbox,val)) =
        case axis of
          (X AxisX) -> bool Nothing (Just region) (R.within_bounds x x_range)
          (Y AxisY) -> bool Nothing (Just region) (R.within_bounds y y_range)
          (Z AxisZ) -> bool Nothing (Just region) (R.within_bounds z z_range)
        where
          region = (bbox,val) 
          x_range = axis_range AxisX bbox
          y_range = axis_range AxisY bbox
          z_range = axis_range AxisZ bbox 

mergeResults :: Either (Collisions BBox3 a) (Maybe [(BBox3, a)]) ->
                Either (Collisions BBox3 a) (Maybe [(BBox3, a)]) ->
                Either (Collisions BBox3 a) (Maybe [(BBox3, a)])
mergeResults (Left (Collisions list)) (Left (Collisions list)) = 
  Left (Collisions list ++ list)
mergeResults (Right list) (Right list) = list <> list
mergeResults list@(Left list') _ = list
mergeResults _ list@(Left list') = list 
