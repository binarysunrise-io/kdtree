{- |
   Module     : Data.Trees.KdTree.Regions.KThree.KThreeTree
   Copyright  : Copyright (c) 2016, Michael Litchard
   License    : BSD3
                       
   Maintainer : Michael Litchard
   Stability  : experimental
   Portability: not portable
                                                         
   This module implements an adaptation of kd-tree for regions.
   http://en.wikipedia.org/wiki/K-d_tree 

-}

{-# LANGUAGE DeriveFoldable, ScopedTypeVariables, Trustworthy, FlexibleContexts,TypeFamilies, TypeSynonymInstances, InstanceSigs, ViewPatterns, FlexibleInstances, DeriveFunctor #-}

module Data.Trees.KdTree.Regions.KThree.KThreeTree where

import Data.Maybe
import Data.Bool
import Data.Vector.V3
import Data.Vector.Class
import qualified Data.List as L
import qualified Data.Foldable as F
import Data.Vector.Fancy 
import qualified Data.BoundingBox.Range as R
import Data.BoundingBox
import Data.BoundingBox.B3 hiding ( min_point
                                  , max_point
                                  , min_point
                                  , bound_corners
                                  , isect)
import Data.Ord
import Data.Either
import Data.Semigroup
import Control.Monad
import Data.Trees.KdTree.Regions.Internal
-- | KdTree adapted to regions, based on
--   Foundations of Multidimensional and Metric Data Structures by Hanan Samet
--

leafSize :: Int
leafSize = 18


-- | 3-D Tree. Needs own module
instance KdTreeRegional BBox3 where
--  type Payload Int = Int
  type Vect BBox3 = Vector3
  type Nearest BBox3 a = Either (Collisions BBox3 a) (Maybe [(Scalar,BBox3, a)])
  data Axes BBox3 = X AxisX | Y AxisY | Z AxisZ deriving Show
  data KdTree BBox3 a
    = KdNode {
        kdLeft     :: KdTree BBox3 a
      , nodeBBox   :: BBox3
      , kdSplit    :: (Axes BBox3,Scalar)
      , overlapped :: [(BBox3, a)] -- can't get Region right
      , kdRight    :: KdTree BBox3 a 
      }
    | KdLeaf (Maybe (Leaf BBox3 a))
    deriving (Foldable,Functor,Show)

  data Collisions BBox3 a = Collisions [(BBox3,a)] deriving (Functor,Show)

  data Leaf BBox3 a = Leaf {
         leafBBox :: BBox3
       , kdleaf   :: [(BBox3, a)] 
       } deriving (Foldable,Functor,Show)

  -- | splitBox splits boundary box along an axis
  splitBox :: Vect BBox3 -> Axes BBox3 -> BBox3 -> (BBox3, BBox3)
  splitBox split (X AxisX) node_bbox = (xbox_left, xbox_right)
    where
      xbox_left  = rangeXYZ leftx_range (rangeY node_bbox) (rangeZ node_bbox)
      xbox_right = rangeXYZ leftx_range (rangeY node_bbox) (rangeZ node_bbox)
      (leftx_range, rightx_range)  = splitRange rangeX (v3x split) node_bbox
  splitBox split (Y AxisY) node_bbox = (ybox_left, ybox_right)
    where
      ybox_left  = rangeXYZ (rangeX node_bbox) lefty_range (rangeZ node_bbox)
      ybox_right = rangeXYZ (rangeX node_bbox) righty_range (rangeZ node_bbox)
      (lefty_range, righty_range) = splitRange rangeY (v3y split) node_bbox
  splitBox split (Z AxisZ) node_bbox = (zbox_left, zbox_right)
    where
      zbox_left  = rangeXYZ (rangeX node_bbox) (rangeY node_bbox) leftz_range
      zbox_right = rangeXYZ (rangeX node_bbox) (rangeY node_bbox) rightz_range
      (leftz_range, rightz_range) = splitRange rangeZ (v3z split) node_bbox

  -- | nearestNeighbor
  --   calculates either the collisions or the nearest non-colliding
  --   first case empty 
  nearestNeighbor :: forall a. KdTree BBox3 a -> BBox3 -> Nearest BBox3 a
  nearestNeighbor (KdLeaf Nothing) _ = Right Nothing
  -- | second case calculates nearest boxes in leaf
  nearestNeighbor (KdLeaf (Just (Leaf leaf_box leaf))) qbox = nearest' 
    where
      nearest' = (nearest (map (findDistance qbox) leaf)) 
  -- | nearestNeighbor case three
  --   (1) seperate out collisions if any
  --   (2) if no collisions determine first list of candidates
  --   (3) take this list and check nearby node boxes for potential
  --       candidates that are closer
  nearestNeighbor (KdNode left node_bbox (axis,scalar) overlapped right) qbox =
    find_nearest
    where
      find_nearest :: Nearest BBox3 a
      find_nearest = 
        mergeResults (nearest (calcDist qbox overlapped)) traverseBoth

      traverseBoth :: Nearest BBox3 a
      traverseBoth = 
        mergeResults left_branch right_branch
       where 
         left_branch  = (traverseBranch BLeft left qbox)
         right_branch = (traverseBranch BRight right qbox)


  -- | toBox builds a list of BBox3 given vector and an offset   
  toBBox :: [(Vect BBox3,BBoxOffset,a)] -> [(BBox3,a)]
  toBBox [] = []
  toBBox xs = L.map toBBox' xs
    where
      toBBox' ((Vector3 x y z),offset,a) = (bbox3,a)
        where
          bbox3 = bound_corners nwu sed
          nwu = Vector3 (x - offset) (y + offset) (z + offset)
          sed = Vector3 (x + offset) (y - offset) (z - offset)

--  toList :: KdTree BBox3 a -> [(BBox3,a)]
--  toList t = F.foldr (:) [] t
  -- | fromList starts with an infinite bounding box
  --            
  fromList :: [(BBox3,a)] -> KdTree BBox3 a 
  fromList [] = KdLeaf Nothing
  fromList aList = fromSubList bbis aList (X AxisX)
    where
      bbis = bound_corners swdCorner neuCorner
      infinity = (read "Infinity") :: Double
      swdCorner = Vector3 (-infinity) (-infinity) (-infinity)
      neuCorner = Vector3 (infinity) (infinity) (infinity)
  

  fromSubList :: forall a. BBox3 -> [(BBox3,a)] -> Axes BBox3 -> KdTree BBox3 a
  fromSubList _ [] _ = KdLeaf Nothing
  fromSubList bbox blist@(length -> l) _ | l <= leafSize =
    KdLeaf (Just (Leaf bbox blist))
    
  fromSubList node_bbox bList axis = node
    where
      node =
        KdNode {
          kdLeft     = fromSubList left_node_bbox front (nextAxis axis)
        , nodeBBox   = node_bbox
        , kdSplit    = (axis, attrib_value axis split)
        , overlapped = 
            mapMaybe (overlap range_func attrib_func split) sortedBoxes
        , kdRight    = fromSubList right_node_bbox back (nextAxis axis)
        } 
      -- | helper functions prepping [(BBox3,Int)] for KdNode
      (left_node_bbox,right_node_bbox) = splitBox split axis node_bbox
      front = take medianIndex removedSplit
      back  = drop medianIndex removedSplit
      removedSplit = (map snd sortedBoxes)
      medianIndex = length sortedBoxes `div` 2
      split = fst (sortedBoxes !! medianIndex)

      nextAxis (X AxisX) = Y AxisY
      nextAxis (Y AxisY) = Z AxisZ
      nextAxis (Z AxisZ) = X AxisX
      
      range_func = case axis of
        (X AxisX) -> rangeX
        (Y AxisY) -> rangeY
        (Z AxisZ) -> rangeZ

      attrib_func = case axis of
        (X AxisX) -> v3x
        (Y AxisY) -> v3y
        (Z AxisZ) -> v3z

      -- | compute and map midpoint of each (BBox3,Int)
      --   for the sort
      midpoints :: [(Vect BBox3,(BBox3,a))]
      midpoints = map midpoint bList

      -- | Sorts based on splitting axis
      sortedBoxes :: [(Vect BBox3,(BBox3,a))]
      sortedBoxes = L.sortBy (sort_by_attrib axis) midpoints


  overlap :: (BBox3 -> R.Range)      ->
             (Vect BBox3 -> Scalar)  ->
             Vector3                 -> 
             (Vect BBox3,(BBox3,a))  -> 
             Maybe (BBox3, a)
  overlap range attrib vector (_,(bbox,val)) = 
    bool Nothing (Just region) (R.within_bounds (attrib vector) (range bbox))
    where 
      region = (bbox,val)

  sort_by_attrib :: Axes BBox3          -> 
                    (Vect BBox3,(BBox3,a)) -> 
                    (Vect BBox3,(BBox3,a)) -> 
                    Ordering
  sort_by_attrib axis p q =
    (attrib_value axis (fst p)) `compare` (attrib_value axis (fst q))

  midpoint :: (BBox3,a) -> (Vect BBox3,(BBox3,a))
  midpoint bp@(b,_) = (midpoint',bp)
    where
      midpoint' = Vector3 minx miny minz
      (Vector3 minx miny minz) = min_point b
      (Vector3 maxx maxy maxz) = max_point b
      midx = (maxx + minx) / 2
      midy = (maxy + miny) / 2
      midz = (maxz + minz) /2

-- | Utilities
traverseBranch :: Branch -> KdTree BBox3 a -> BBox3 -> Nearest BBox3 a
traverseBranch _ (KdLeaf Nothing) _ = Right Nothing 
traverseBranch _ (KdLeaf (Just(Leaf leaf_bbox leaf))) qbox =
  case (isect qbox leaf_bbox) of
    Just _ -> candidates
    _      -> Right Nothing
  where
    candidates = (nearest (calcDist qbox leaf))
traverseBranch branch node@(KdNode left node_bbox _ overlap right) qbox =
  case (isect qbox node_bbox) of
    Just _ -> intersected
    _      -> Right Nothing
  where
    intersected = case candidates of
      Left cand         -> Left cand
      Right (Just cand) -> verify qbox nextBranch cand
      Right _           -> Right Nothing
    candidates = 
      mergeResults (nearest (calcDist qbox overlap)) $
      mergeResults (nearestNeighbor left qbox) 
                   (nearestNeighbor right qbox)

    nextBranch = case branch of
      BLeft -> right
      BRight -> left
          
verify :: forall a. BBox3                -> 
                    KdTree BBox3 a       ->
                    [(Scalar, BBox3, a)] ->
                    Nearest BBox3 a
verify _ (KdLeaf Nothing) _ = Right Nothing
verify qbox (KdLeaf (Just (Leaf _ leaf))) candidates@((d,_,_):_)
  | d `isLess` d' = Right $ Just candidates
  | otherwise     = d'
  where
    d' :: Nearest BBox3 a
    d' = nearest (calcDist qbox leaf)
    isLess :: Scalar -> Nearest BBox3 a -> Bool
    isLess d1 (Left _) = False -- should never happen
    isLess d1 (Right Nothing) = True -- should never happen
    isLess d1 (Right (Just ((d2,_,_):_))) = d1 < d2
          
verify qbox (KdNode left node_bbox _ overlap right) candidates@(x:_) =
  case (evaluateDistanceFromNode x qbox node_bbox) of
    True  -> Right $ Just candidates -- orig list is closer
    False -> mergeResults (nearest (calcDist qbox overlap)) mergeTraversals 
  where
    mergeTraversals = mergeResults verifyLeft verifyRight
    verifyLeft      = (verify qbox left candidates)
    verifyRight     = (verify qbox right candidates)

-- | mergeResults
--   mergeResults favors Left collisions over Right nearest
mergeResults :: Nearest BBox3 a -> Nearest BBox3 a -> Nearest BBox3 a
mergeResults (Left (Collisions xs)) (Left (Collisions xs')) =
  Left (Collisions (xs <> xs'))
mergeResults (Right (Just xs)) (Right (Just xs')) =
  Right (Just (xs <> xs'))
mergeResults xs@(Left _) _ = xs
mergeResults _ xs@(Left _) = xs

-- | nearest produces intermediate value for
--
nearest :: [(Scalar, BBox3, a)] -> (Nearest BBox3 a)
nearest dists = case (partitionDistances (map findDistances dists)) of
  Right candidates -> Right (Just (filtered candidates)) 
  Left collisions  -> Left collisions
  where
    filtered candidates = filterCandidates $ L.sortBy distance candidates

-- | partitionDistances only returns non-collided if collided don't exist
partitionDistances :: [Either (BBox3,a) (Scalar,BBox3,a)] ->
                      Either (Collisions BBox3 a) [(Scalar,BBox3, a)]
partitionDistances e = case (partitionEithers e) of
  ([],distances) -> Right distances
  (collisions,_) -> Left (Collisions collisions)

-- | evaluateDistanceFromNode
--   evaluateDistanceFromNode compares distance of query box
--   to node bounding box
evaluateDistanceFromNode :: (Scalar, BBox3, a) -> BBox3 -> BBox3 -> Bool
evaluateDistanceFromNode  (dist1,_,_) bx nbx = dist1 < dist2
  where
    dist2 = sqrt ( (distx'^2) + (disty'^2) + (distz'^2) )
    distx' = boxAxisDistance rangeX bx nbx
    disty' = boxAxisDistance rangeY bx nbx
    distz' = boxAxisDistance rangeZ bx nbx

calcDist qbox overlapped = map (findDistance qbox) overlapped 

  -- | findDistance calculates the Euclidean distance between
  --   the query box and a candidate
findDistance :: BBox3     -> 
                (BBox3, a) -> 
                (Scalar, BBox3, a) 
findDistance bbox1 (bbox2,a) = (edist,bbox2,a)
  where
    edist = sqrt ( (distx'^2) + (disty'^2) + (distz'^2) )
    distx' = boxAxisDistance rangeX bbox1 bbox2
    disty' = boxAxisDistance rangeY bbox1 bbox2
    distz' = boxAxisDistance rangeZ bbox1 bbox2

attrib_value :: Axes BBox3 -> Vect BBox3 -> Scalar
attrib_value (X AxisX) vect = get_coord AxisX vect
attrib_value (Y AxisY) vect = get_coord AxisY vect
attrib_value (Z AxisZ) vect = get_coord AxisZ vect

