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

{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies, InstanceSigs #-}

module Data.Trees.KdTree.Regions.Internal where

import Data.Maybe
import Data.Vector.V3
import Data.Vector.Class
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Vector.Fancy
import Data.BoundingBox
import Data.Ord
-- | KdTree adapted to regions, based on
--   Foundations of Multidimensional and Metric Data Structures by Hanan Samet
--


class ( BoundingBox bbox
      , Vector vect
      , Vector center  -- vect ~ center
       
      ) => KdTreeRegional bbox vect center payload where
  
  type Payload payload :: *
  data Axes vect :: *
  data KdTree vect bbox payload :: * -> *
  data Region bbox center :: * -> *

  -- | fromList builds KdTree given list of Vector/payload pairs
  fromList :: [(vect,payload)] -> KdTree vect bbox payload center

  -- | fromSubList
  fromSubList :: [(vect,payload)] -> Axes Vector3 -> KdTree vect bbox payload center

  -- | toList generates list of Vector/payload pairs, given a KdTree
  toList :: KdTree vect bbox payload center -> [(vect,payload)]
  -- | nearestNeighbor returns the nearest neighbor of bbox in tree.
  nearestNeighbor :: KdTree vect bbox payload center -> bbox -> Maybe bbox
  -- | nearNeighbors kdtree r bbox returns all neighbors within 
  --   distance r from bbox in tree.
  nearNeighbors :: KdTree vect bbox payload center -> 
                   Scalar                          ->
                   bbox                            ->
                   [bbox]
  -- | kNearestNeighbors tree k bbox returns the k closest points
  --   to bbox within tree.
  kNearestNeighbors :: KdTree vect bbox payload center -> Int -> bbox -> [bbox]
  -- | remove t (v,p) removes (v,p) from t
  remove :: KdTree vect bbox payload center -> 
            (vect,payload)                  ->
            KdTree vect bbox payload center

-- | Demo instance. To Be moved to DocTest
instance KdTreeRegional BBox3 Vector3 Vector3 Int where
  type Payload Int = Int
  data Axes Vector3 = X AxisX | Y AxisY | Z AxisZ deriving Show
  data KdTree Vector3 BBox3 Int Vector3
    = KdNode {
        kdLeft  :: KdTree Vector3 BBox3 Int Vector3
      , kdSplit :: (Axes Vector3,Scalar)
      , kdRight :: KdTree Vector3 BBox3 Int Vector3
      }
    | KdLeaf (Maybe (Region BBox3 Vector3 Int))
    deriving Show

  data Region BBox3 Vector3 Int = Region
    { region  :: BBox3
    , center  :: Vector3
    , payload :: Int
    } deriving Show

  -- |

  fromList :: [(Vector3,Payload Int)] -> KdTree Vector3 BBox3 Int Vector3
  fromList [] = KdLeaf Nothing
  fromList aList = fromSubList aList (X AxisX)

  -- | The Vector3 represents the center from which the Bounding
  --   Box is built. 
  fromSubList :: [(Vector3,Payload Int)] ->
                 Axes Vector3            ->
                 KdTree Vector3 BBox3 Int Vector3
  fromSubList ((center@(Vector3 x y z),payload):[]) _ =
    KdLeaf (Just region)
    where
       region = Region bbox center payload
       bbox   = bound_corners nwu sed
       nwu    = Vector3 nwux nwuy nwuz
       sed    = Vector3 sedx sedy sedz
       nwux   = x - 1
       nwuy   = y + 1
       nwuz   = z + 1
       sedx   = x + 1
       sedy   = y - 1
       sedz   = z - 1
    
  fromSubList aList axis = node
    where

      sortedPoints = 
        L.sortBy (\a b -> (attrib_value (fst a)) `compare` (attrib_value (fst b))) aList

      medianIndex = length sortedPoints `div` 2
      
      pivot = sortedPoints !! medianIndex
      attrib_value = get_coord_wrapper axis
      get_coord_wrapper (X AxisX) = get_coord AxisX
      get_coord_wrapper (Y AxisY) = get_coord AxisY
      get_coord_wrapper (Z AxisZ) = get_coord AxisZ

      nextAxis (X AxisX) = Y AxisY
      nextAxis (Y AxisY) = Z AxisZ
      nextAxis (Z AxisZ) = X AxisX
 
      node =
        KdNode {
          kdLeft  =
            fromSubList (take medianIndex sortedPoints) (nextAxis axis)
        , kdSplit = (axis, attrib_value (fst pivot))
        , kdRight = 
            fromSubList (drop medianIndex sortedPoints) (nextAxis axis)
        }
   
      nextAxis (X AxisX) = Y AxisY
      nextAxis (Y AxisY) = Z AxisZ
      nextAxis (Z AxisZ) = X AxisX      
