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


class ( BoundingBox box
      , Vector vect
      , Vector center , vect ~ center
      ) => KdTreeRegional box vect center payload where
  data Axes vect :: *
  data KdTree vect box payload :: * -> *
  data Region box center :: * -> *

instance KdTreeRegional BBox3 Vector3 Vector3 Int where 
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
  
  

