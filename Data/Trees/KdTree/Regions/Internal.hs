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

{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances, InstanceSigs, ViewPatterns, FunctionalDependencies #-}

module Data.Trees.KdTree.Regions.Internal 
  ( BBoxOffset
  , LeftRange
  , RightRange
  , KdTreeRegional (..)
  
  ) where

import Data.Vector.V3
import Data.Vector.Class
import Data.Vector.Fancy 
import Data.BoundingBox
import qualified Data.BoundingBox.Range as R
-- | KdTree adapted to regions, based on
--   Foundations of Multidimensional and Metric Data Structures by Hanan Samet
--
type BBoxOffset = Scalar
-- type Distance   = Scalar
type LeftRange  = R.Range
type RightRange = R.Range
class ( BoundingBox bbox, Vector vect) => 
      KdTreeRegional bbox vect | bbox -> vect where
  
--  type Payload payload :: *

  data Axes bbox :: *
  data KdTree bbox :: * -> *
  data Collisions bbox :: * -> * 
  type Region bbox :: * -> * -- needs work
  type Vect bbox :: *
 
  findDistance :: bbox -> (bbox, a) -> (Scalar, bbox, a)

  findNearest :: [(Scalar,bbox,a)]                             -> 
                 Either (Collisions bbox a) [(Scalar,bbox,a)]


  boxAxisDistance :: Axes bbox -> bbox -> bbox -> Scalar
  -- | SplitRange splits a range on a given axis
  splitRange :: vect -> Axes bbox -> bbox -> (R.Range, R.Range)
  -- | SplitBox splits a node's bounding box along a given axis, at the split
  splitBox :: vect -> Axes bbox -> bbox -> (bbox,bbox)
  -- | bbis is a Bounding Box of Infinite Space
--  bbis :: bbox

  toBBox :: [(vect,BBoxOffset,a)] -> [(bbox,a)]

  -- | fromList builds KdTree given list of bboxes/payload pairs
  fromList :: [(bbox,a)] -> KdTree bbox a 

  -- | fromSubList
  fromSubList :: bbox -> [(bbox,a)] -> Axes bbox -> KdTree bbox a 

  -- | toList generates list of bbox/payload pairs, given a KdTree
  toList :: KdTree bbox a -> [(bbox,a)]
  -- | nearestNeighbor returns the nearest neighbor of bbox in tree.
  nearestNeighbor :: KdTree bbox a                        -> 
                     bbox                                 -> 
                     Either (Collisions bbox a) (Maybe [(Scalar,bbox,a)])
  -- | nearNeighbors kdtree r bbox returns all neighbors within 
  --   distance r from bbox in tree.
  nearNeighbors :: KdTree bbox a -> Scalar -> bbox -> Maybe [(Scalar,bbox,a)]
  -- | kNearestNeighbors tree k bbox returns the k closest points
  --   to bbox within tree.
  kNearestNeighbors :: KdTree bbox a -> Int -> bbox -> [(Scalar,bbox,a)]

  evalBox :: bbox -> Axes bbox -> Scalar -> Bool

  -- | insert t (b,a) inserts (b,a) in t
  insert :: KdTree bbox a -> (bbox,a) -> KdTree bbox a
  -- | remove t (b,a) removes (b,a) from t
  remove :: KdTree bbox a -> (bbox,a) -> KdTree bbox a

  midpoint :: (bbox,a) -> (vect, (bbox,a))

  attrib_value :: Axes bbox -> vect -> Scalar

  sort_by_attrib :: Axes bbox         -> 
                    (vect, (bbox, a)) -> 
                    (vect,(bbox,a))   -> 
                    Ordering

--  overlaps :: Axes bbox -> vect -> [(vect,(bbox,a))]

--  overlap :: Axes bbox -> vect -> (vect,(bbox,a))
  
