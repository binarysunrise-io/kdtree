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
  , Branch (..)
  , boxAxisDistance
  , splitRange
  , findDistances
  , filterCandidates
  , evalBox
  , distance
  ) where

import Data.Vector.V3
import Data.Vector.Class
import Data.Vector.Fancy 
import Data.BoundingBox
import qualified Data.List as L
import qualified Data.BoundingBox.Range as R
-- | KdTree adapted to regions, based on
--   Foundations of Multidimensional and Metric Data Structures by Hanan Samet
--
type BBoxOffset = Scalar
data Branch = BLeft | BRight deriving Show
-- type Distance   = Scalar
type LeftRange  = R.Range
type RightRange = R.Range

class ( BoundingBox bbox) => 
      KdTreeRegional bbox where
  
--  type Payload payload :: *

  data Axes bbox :: *
  data KdTree bbox :: * -> *
  data Leaf bbox :: * -> *
  data Collisions bbox :: * -> * 
  type Region bbox a :: * -- needs work
  type Vect bbox :: *
  type Nearest bbox a :: *
 
  -- | SplitBox splits a node's bounding box along a given axis, at the split
  splitBox :: Vect bbox -> Axes bbox -> bbox -> (bbox,bbox)

  overlap :: (bbox -> R.Range)     -> 
             (Vect bbox -> Scalar) ->
             Vect bbox             ->
             (Vect bbox,(bbox,a))  -> 
             Maybe (bbox,a)

  toBBox :: [(Vect bbox,BBoxOffset,a)] -> [(bbox,a)]

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

  findNearest :: KdTree bbox a -> bbox -> [(Scalar,bbox,a)]

  -- | insert t (b,a) inserts (b,a) in t
  insert :: KdTree bbox a -> (bbox,a) -> KdTree bbox a
  -- | remove t (b,a) removes (b,a) from t
  remove :: KdTree bbox a -> (bbox,a) -> KdTree bbox a

  midpoint :: (bbox,a) -> (Vect bbox, (bbox,a))

  sort_by_attrib :: Axes bbox              -> 
                    (Vect bbox, (bbox, a)) -> 
                    (Vect bbox,(bbox,a))   -> 
                    Ordering

--  overlaps :: Axes bbox -> vect -> [(vect,(bbox,a))]

--  overlap :: Axes bbox -> vect -> (vect,(bbox,a))

boxAxisDistance :: (BoundingBox a) => (a -> R.Range) -> a -> a -> Scalar
boxAxisDistance findRange bbox1 bbox2
  | min1 > max2 = min1 - max2
  | min2 > max1 = min2 - max1
  | otherwise     = 0 -- collision
    where
      min1 = R.min_point range1
      max1 = R.max_point range1
      min2 = R.min_point range2
      max2 = R.max_point range2
      range1 = findRange bbox1
      range2 = findRange bbox2

splitRange :: (BoundingBox a) => 
              (a -> R.Range)  -> 
              Scalar          ->
              a               ->
              (LeftRange,RightRange)
splitRange findRange split_attrib node_bbox = (left_range,right_range)
  where
    left_range = R.Range min_point split_attrib
    right_range = R.Range split_attrib max_point
    min_point   = R.min_point (findRange node_bbox)
    max_point   = R.max_point (findRange node_bbox)

findDistances :: (BoundingBox b)    =>
                 (Scalar, b, a) -> 
                 Either (b,a) (Scalar,b,a)
findDistances a@(dist,box,value)
  | dist == 0 = Left (box,value) -- collision!
  | otherwise = Right a

evalBox :: (BoundingBox a) => (a -> R.Range) -> a -> Scalar -> Bool
evalBox findRange bbox scalar = scalar > R.max_point (findRange bbox)

-- | filterCandidates
--   returns all eqidistant nearest
filterCandidates :: (BoundingBox b) => [(Scalar,b,a)] -> [(Scalar,b,a)]
filterCandidates dists = filterCandidates' (L.sortBy distance dists)

filterCandidates' :: (BoundingBox b) => [(Scalar,b,a)] -> [(Scalar,b,a)]
filterCandidates' (v1@(dist1,_,_):v2@(dist2,_,_):xs)
  | dist1 == dist2 = v1:filterCandidates' (v2:xs)
  | otherwise      = v1:filterCandidates' []

distance (d1,_,_) (d2,_,_) = d1 `compare` d2 
