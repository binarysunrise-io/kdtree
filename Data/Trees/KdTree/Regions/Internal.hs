{- |
   Module     : Data.Trees.KdTree.Regions.Internal
   Copyright  : Copyright (c) 2016, Michael Litchard
   License    : BSD3
                       
   Maintainer : Michael Litchard
   Stability  : experimental
   Portability: not portable
                                                         
   This module implements an adaptation of kd-tree for regions.
   http://en.wikipedia.org/wiki/K-d_tree 

-}

{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances, InstanceSigs #-}

module Data.Trees.KdTree.Regions.Internal where

import Data.Maybe
import Data.Bool
import Data.Vector.V3
import Data.Vector.Class
import qualified Data.List as L
import Data.Vector.Fancy 
import qualified Data.BoundingBox.Range as R
import Data.BoundingBox
import Data.Ord
import qualified Data.List.NonEmpty as NE
-- | KdTree adapted to regions, based on
--   Foundations of Multidimensional and Metric Data Structures by Hanan Samet
--

type BBoxOffset = Scalar

class ( BoundingBox bbox
      , Vector vect
      ) => KdTreeRegional bbox vect payload where
  
  type Payload payload :: *
  data Axes vect :: *
  data KdTree bbox payload :: *
  data Region bbox payload :: *
   
  toBBox :: [(vect,BBoxOffset,payload)] -> 
            [(bbox,payload)]
  -- | fromList builds KdTree given list of bboxes/payload pairs
  fromList :: [(bbox,payload)] -> KdTree bbox payload 

  -- | fromSubList
  fromSubList :: [(bbox,payload)] -> Axes vect -> KdTree bbox payload 

  -- | toList generates list of bbox/payload pairs, given a KdTree
  toList :: KdTree bbox payload -> [(bbox,payload)]
  -- | nearestNeighbor returns the nearest neighbor of bbox in tree.
  nearestNeighbor :: KdTree bbox payload -> bbox -> Maybe bbox
  -- | nearNeighbors kdtree r bbox returns all neighbors within 
  --   distance r from bbox in tree.
  nearNeighbors :: KdTree bbox payload -> 
                   Scalar              ->
                   bbox                ->
                   [bbox]
  -- | kNearestNeighbors tree k bbox returns the k closest points
  --   to bbox within tree.
  kNearestNeighbors :: KdTree bbox payload -> Int -> bbox -> [bbox]
  -- | remove t (v,p) removes (v,p) from t
  remove :: KdTree bbox payload -> 
            (bbox,payload)      ->
            KdTree bbox payload 

-- | Demo instance. To Be moved to DocTest
instance KdTreeRegional BBox3 Vector3 Int where
  type Payload Int = Int
  data Axes Vector3 = X AxisX | Y AxisY | Z AxisZ deriving Show
  data KdTree BBox3 Int 
    = KdNode {
        kdLeft     :: KdTree BBox3 Int 
      , kdSplit    :: (Axes Vector3,Scalar)
      , overlapped :: [(Region BBox3 Int)]
      , kdRight    :: KdTree BBox3 Int 
      }
    | KdLeaf (Maybe (Region BBox3 Int))
    deriving Show

  data Region BBox3 Int = Region
    { region  :: BBox3
    , payload :: Int
    } deriving Show

  -- |
  toBBox :: [(Vector3,BBoxOffset,Payload Int)] ->
            [(BBox3,Payload Int)]
  toBBox [] = []
  toBBox xs = L.map toBBox' xs
    where
      toBBox' ((Vector3 x y z),offset,payload) = (bbox3,payload)
        where
          bbox3 = bound_corners nwu sed
          nwu = Vector3 (x - offset) (y + offset) (z + offset)
          sed = Vector3 (x + offset) (y - offset) (z - offset)
           
  fromList :: [(BBox3,Payload Int)] -> KdTree BBox3 Int 
  fromList [] = KdLeaf Nothing
  fromList aList = fromSubList aList (X AxisX)

  fromSubList :: [(BBox3,Payload Int)] ->
                 Axes Vector3          ->
                 KdTree BBox3 Int 
  fromSubList ((bbox,payload):[]) _ =
    KdLeaf (Just region)
    where
      region = Region bbox payload
    
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
      midpoints :: [(Vector3,(BBox3,Int))]
      midpoints = map midpoint bList

      midpoint :: (BBox3,Int) -> (Vector3,(BBox3,Int))
      midpoint bp@(b,_) = (midpoint',bp)
        where
          midpoint' = Vector3 minx miny minz
          (Vector3 minx miny minz) = min_point b
          (Vector3 maxx maxy maxz) = max_point b
          midx = (maxx + minx) / 2
          midy = (maxy + miny) / 2
          midz = (maxz + minz) /2 

      -- | Sorts based on splitting axis
      sortedBoxes :: [(Vector3,(BBox3,Int))]
      sortedBoxes = 
        L.sortBy sort_by_attrib midpoints
  
      sort_by_attrib :: (Vector3,(BBox3,Int)) ->
                        (Vector3,(BBox3,Int)) -> 
                        Ordering
      sort_by_attrib a b =
        (attrib_value axis (fst a)) `compare` (attrib_value axis (fst b)) 
      attrib_value (X AxisX) vect = get_coord AxisX vect
      attrib_value (Y AxisY) vect = get_coord AxisY vect
      attrib_value (Z AxisZ) vect = get_coord AxisZ vect

      -- | Computes BBoxs overlapping split
      overlaps :: Axes Vector3  -> 
                  Vector3       -> 
                  [(Vector3,(BBox3,Int))] -> 
                  [Region BBox3 Int]
      overlaps axis split sortedBoxes =
        mapMaybe (overlap axis split) sortedBoxes

      overlap :: Axes Vector3 -> 
                 Vector3 -> 
                 (Vector3,(BBox3,Int)) -> 
                 Maybe (Region BBox3 Int)
      overlap axis (Vector3 x y z) (_,(bbox,payload)) =
        case axis of
          (X AxisX) -> bool Nothing (Just region) (R.within_bounds x x_range)
          (Y AxisY) -> bool Nothing (Just region) (R.within_bounds y y_range)
          (Z AxisZ) -> bool Nothing (Just region) (R.within_bounds z z_range)
        where
          region = Region bbox payload
          x_range = axis_range AxisX bbox
          y_range = axis_range AxisY bbox
          z_range = axis_range AxisZ bbox 
