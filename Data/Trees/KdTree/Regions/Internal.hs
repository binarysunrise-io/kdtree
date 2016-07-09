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

{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}

module Data.Trees.KdTree.Regions.Internal where

import Data.Maybe
import Data.Vector.V3
import Data.Vector.Class
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Vector.Fancy
import Data.BoundingBox
-- | KdTree adapted to regions, based on
--   Foundations of Multidimensional and Metric Data Structures by Hanan Samet
--


class ( BoundingBox bbox
      , Vector vect
      , Vector center , vect ~ center
      ) => KdTreeRegional bbox vect center where
  data Axes vect :: *
  data KdTree vect bbox payload :: * -> *
  data Region bbox center :: * -> *

  -- | fromList builds KdTree given list of Vector/payload pairs
  fromList          :: [(vect,payload)] -> KdTree vect bbox payload center
  -- | toList generates list of Vector/payload pairs, given a KdTree
  toList            :: KdTree vect bbox payload center -> [(vect,payload)]
  -- | nearestNeighbor returns the nearest neighbor of bbox in tree.
  nearestNeighbor   :: KdTree vect bbox payload center -> bbox -> Maybe bbox
  -- | nearNeighbors kdtree r bbox returns all neighbors within distance r from bbox in tree.
  nearNeighbors     :: KdTree vect bbox payload center -> Scalar -> bbox -> [bbox]
  -- | kNearestNeighbors tree k bbox returns the k closest points to bbox within tree.
  kNearestNeighbors :: KdTree vect bbox payload center -> Int -> bbox -> [bbox]
  -- | remove t (v,p) removes (v,p) from t
  remove :: KdTree vect bbox payload center -> (vect,payload) -> KdTree vect bbox payload center
  -- | nextAxis gives next Axis in rotation
  nextAxis :: Axes vect -> Axes vect

instance KdTreeRegional BBox3 Vector3 Vector3 where 
  data Axes Vector3 = X AxisX | Y AxisY | Z AxisZ deriving Show
  data KdTree Vector3 BBox3 Int Vector3
    = KdNode {
        kdLeft  :: KdTree Vector3 BBox3 Int Vector3
      , kdPivot :: Vector3
      , kdRight :: KdTree Vector3 BBox3 Int Vector3
      , kdAxis  :: Axes Vector3 
      }
    | KdLeaf (Maybe (Region BBox3 Vector3 Int))
    deriving Show

  data Region BBox3 Vector3 Int = Region
    { region  :: BBox3
    , center  :: Vector3
    , payload :: Int
    } deriving Show
  
  

