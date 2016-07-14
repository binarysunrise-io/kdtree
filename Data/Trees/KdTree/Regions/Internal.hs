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

{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances, InstanceSigs, ViewPatterns #-}

module Data.Trees.KdTree.Regions.Internal 
  ( BBoxOffset
  , KdTreeRegional (..)
  , Region
  ) where

import Data.Vector.V3
import Data.Vector.Class
import Data.Vector.Fancy 
import Data.BoundingBox
-- | KdTree adapted to regions, based on
--   Foundations of Multidimensional and Metric Data Structures by Hanan Samet
--
type BBoxOffset = Scalar
type Region a = [(BBox3, a)]
class ( BoundingBox bbox, Vector vect) => KdTreeRegional bbox vect where
  
--  type Payload payload :: *
  data Axes vect :: *
  data KdTree bbox :: * -> *
  data Collisions bbox :: * -> * 
  toBBox :: [(vect,BBoxOffset,a)] -> [(bbox,a)]

  -- | fromList builds KdTree given list of bboxes/payload pairs
  fromList :: [(bbox,a)] -> KdTree bbox a 

  -- | fromSubList
  fromSubList :: [(bbox,a)] -> Axes vect -> KdTree bbox a 

  -- | toList generates list of bbox/payload pairs, given a KdTree
  toList :: KdTree bbox a -> [(bbox,a)]
  -- | nearestNeighbor returns the nearest neighbor of bbox in tree.
  nearestNeighbor :: KdTree bbox a                        -> 
                     bbox                                 -> 
                     Maybe (Region a)
  -- | nearNeighbors kdtree r bbox returns all neighbors within 
  --   distance r from bbox in tree.
  nearNeighbors :: KdTree bbox a -> Scalar -> bbox -> [bbox]
  -- | kNearestNeighbors tree k bbox returns the k closest points
  --   to bbox within tree.
  kNearestNeighbors :: KdTree bbox a -> Int -> bbox -> [bbox]

  -- | insert t (b,a) inserts (b,a) in t
  insert :: KdTree bbox a -> (bbox,a) -> KdTree bbox a
  -- | remove t (b,a) removes (b,a) from t
  remove :: KdTree bbox a -> (bbox,a) -> KdTree bbox a
