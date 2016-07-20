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

{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances, InstanceSigs, ViewPatterns, FlexibleInstances, DeriveFunctor #-}

module Data.Trees.KdTree.Regions.KThree.KThreeTree where

import Data.Maybe
import Data.Bool
import Data.Vector.V3
import Data.Vector.Class
import qualified Data.List as L
import Data.Vector.Fancy 
import qualified Data.BoundingBox.Range as R
import Data.BoundingBox
import Data.BoundingBox.B3 hiding ( min_point
                                  , max_point
                                  , min_point
                                  , bound_corners)
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
instance KdTreeRegional BBox3 Vector3 where
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
    | KdLeaf (Maybe [(BBox3, a)])
    deriving Show

  data Collisions BBox3 a = Collisions [(BBox3,a)] deriving (Functor,Show)

  -- | findDistance calculates the Euclidean distance between
  --   the query box and a candidate
  findDistance :: BBox3     -> 
                 (BBox3, a) -> 
                 (Scalar, BBox3, a) 
  findDistance bbox1 (bbox2,a) = (edist,bbox2,a)
    where
      edist = sqrt ( (distx'^2) + (disty'^2) + (distz'^2) )
      distx' = boxAxisDistance (X AxisX) bbox1 bbox2
      disty' = boxAxisDistance (Y AxisY) bbox1 bbox2
      distz' = boxAxisDistance (Z AxisZ) bbox1 bbox2

  -- | boxAxisDistance calculates distance between two boxes on an axis
  boxAxisDistance :: Axes BBox3 -> BBox3 -> BBox3 -> Scalar
  boxAxisDistance (X AxisX) bbox1 bbox2
    | minx1 > maxx2 = minx1 - maxx2
    | minx2 > maxx1 = minx2 - maxx1
    | otherwise     = 0 -- collision
      where
        minx1 = R.min_point xrange1 
        maxx1 = R.max_point xrange1 
        minx2 = R.min_point xrange2
        maxx2 = R.max_point xrange2
        xrange1 = axis_range AxisX bbox1
        xrange2 = axis_range AxisX bbox2
  boxAxisDistance (Y AxisY) bbox1 bbox2
    | miny1 > maxy2 = miny1 - maxy2
    | miny2 > maxy1 = miny2 - maxy1
    | otherwise     = 0 -- collision
      where
        miny1 = R.min_point yrange1
        maxy1 = R.max_point yrange1
        miny2 = R.min_point yrange2
        maxy2 = R.max_point yrange2
        yrange1 = axis_range AxisY bbox1
        yrange2 = axis_range AxisY bbox2
  boxAxisDistance (Z AxisZ) bbox1 bbox2
    | minz1 > maxz2 = minz1 - maxz2
    | minz2 > maxz1 = minz2 - maxz1
    | otherwise     = 0 -- collision
      where
        minz1   = R.min_point zrange1
        maxz1   = R.max_point zrange1
        minz2   = R.min_point zrange2
        maxz2   = R.max_point zrange2
        zrange1 = axis_range AxisZ bbox1
        zrange2 = axis_range AxisZ bbox2
         
      
  -- | splitRange splits a range on a given axis
  splitRange :: Vector3 -> Axes BBox3 -> BBox3 -> (LeftRange,RightRange)
  splitRange split (X AxisX) node_bbox = (leftx_range, rightx_range)
    where
      leftx_range  = R.Range minx (v3x split)
      rightx_range = R.Range (v3x split) maxx
      minx         = R.min_point xrange
      maxx         = R.max_point xrange
      xrange       = axis_range AxisX node_bbox
  splitRange split (Y AxisY) node_bbox = (lefty_range, righty_range)
    where
      lefty_range  = R.Range miny (v3y split)
      righty_range = R.Range (v3y split) maxy
      miny         = R.min_point yrange
      maxy         = R.max_point yrange
      yrange       = axis_range AxisY node_bbox
  splitRange split (Z AxisZ) node_bbox = (leftz_range, rightz_range)
    where
      leftz_range  = R.Range minz (v3z split)
      rightz_range = R.Range (v3z split) maxz
      minz         = R.min_point zrange
      maxz         = R.max_point zrange
      zrange       = axis_range AxisZ node_bbox

  -- | splitBox splits boundary box along an axis
  splitBox :: Vector3 -> Axes BBox3 -> BBox3 -> (BBox3, BBox3)
  splitBox split axis@(X AxisX) node_bbox = (xbox_left, xbox_right)
    where
      xbox_left  = rangeXYZ leftx_range (rangeY node_bbox) (rangeZ node_bbox)
      xbox_right = rangeXYZ leftx_range (rangeY node_bbox) (rangeZ node_bbox)
      (leftx_range, rightx_range)  = splitRange split axis node_bbox
  splitBox split axis@(Y AxisY) node_bbox = (ybox_left, ybox_right)
    where
      ybox_left  = rangeXYZ (rangeX node_bbox) lefty_range (rangeZ node_bbox)
      ybox_right = rangeXYZ (rangeX node_bbox) righty_range (rangeZ node_bbox)
      (lefty_range, righty_range) = splitRange split axis node_bbox
  splitBox split axis@(Z AxisZ) node_bbox = (zbox_left, zbox_right)
    where
      zbox_left  = rangeXYZ (rangeX node_bbox) (rangeY node_bbox) leftz_range
      zbox_right = rangeXYZ (rangeX node_bbox) (rangeY node_bbox) rightz_range
      (leftz_range, rightz_range) = splitRange split axis node_bbox

  -- | evalBox returns a Bool that determines which branch of a Node 
  --   to traverse
  evalBox :: BBox3 -> Axes BBox3 -> Scalar -> Bool
  evalBox bbox axis scalar = case axis of
    (X AxisX) -> scalar > R.max_point (axis_range AxisX bbox)
    (Y AxisY) -> scalar > R.max_point (axis_range AxisY bbox)
    (Z AxisZ) -> scalar > R.max_point (axis_range AxisZ bbox)

  -- | nearestNeighbor
  --   calculates either the collisions or the nearest non-colliding
  --   first case empty 
  nearestNeighbor :: KdTree BBox3 a -> BBox3 -> Nearest BBox3 a
  nearestNeighbor (KdLeaf Nothing) _ = Right Nothing
  -- | second case calculates nearest boxes in leaf
  nearestNeighbor (KdLeaf (Just leaf)) bbox = case nearest' of
    (Left collision) -> Left collision
    Right nearest'   -> Right (Just nearest') 
    where
      nearest' = (nearest (map (findDistance bbox) leaf)) 

  -- | nearestNeighbor third case 
  --   in the event there are no leaves connected to Node
  nearestNeighbor
    (KdNode (KdLeaf Nothing) node_bbox _ overlapped (KdLeaf Nothing)) bbox =
      case nearest' of
        (Left collision) -> Left collision
        Right nearest''  -> Right (Just nearest'')
      where
        nearest' = (nearest (map (findDistance bbox) overlapped))
  -- | nearestNeighbor case four
  --   (1) seperate out collisions if any
  --   (2) if no collisions determine first list of candidates
  --   (3) take this list and check nearby node boxes for potential
  --       candidates that are closer
  nearestNeighbor (KdNode left node_bbox (axis,scalar) overlapped right) bbox =
    case (findNearest) of
      collisions@(Left _)       -> collisions
      (Right (Just candidates)) -> case (nearest candidates) of
                                     (Left collision) -> Left collision
                                     Right nearest''  -> Right (Just nearest'')
      _                         -> Right Nothing
    where 
      -- | findNearest traverses either one or both branches 
      findNearest = case (bbox `elem` (fmap fst overlapped)) of
        True  -> mergeResults (Right (Just overlappedDist)) traverseBoth
        False -> traverseOneOrBoth

      overlappedDist = map (findDistance bbox) overlapped

--      traverseBoth :: Nearest BBox3 a
      traverseBoth = mergeResults traverseLeft traverseRight

--      traverseOneOrBoth :: [(Scalar,BBox3,a)]
      traverseOneOrBoth = case (evalBox bbox axis scalar) of
        True -> evaluateTraversal traverseRight bbox left
        False -> evaluateTraversal traverseLeft bbox right

      -- | evaluateTraversal
      --   when Right is evaluating a KdNode the candidates
      --   are compared against the distance of either the left or right branch
      evaluateTraversal :: Nearest BBox3 a -> 
                           BBox3           ->
                           KdTree BBox3 a  ->
                           Nearest BBox3 a
      evaluateTraversal collisions@(Left _) _ _ = collisions
      evaluateTraversal candidates@(Right _) _ (KdLeaf _) = candidates
      evaluateTraversal candidates@(Right (Just (x:xs))) box n@(KdNode l nbx' _ _ r) =
        case (evaluateDistanceFromNode x box nbx') of
          True  -> nearestNeighbor n bbox
          False -> candidates
      evaluateTraversal _ _ _ = Right Nothing -- This should never happen

      traverseLeft  = nearestNeighbor left bbox
      traverseRight = nearestNeighbor right bbox
  
  -- | toBox builds a list of BBox3 given vector and an offset   
  toBBox :: [(Vector3,BBoxOffset,a)] -> [(BBox3,a)]
  toBBox [] = []
  toBBox xs = L.map toBBox' xs
    where
      toBBox' ((Vector3 x y z),offset,a) = (bbox3,a)
        where
          bbox3 = bound_corners nwu sed
          nwu = Vector3 (x - offset) (y + offset) (z + offset)
          sed = Vector3 (x + offset) (y - offset) (z - offset)

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
  

  fromSubList :: BBox3 -> [(BBox3,a)] -> Axes BBox3 -> KdTree BBox3 a
  fromSubList _ [] _ = KdLeaf Nothing
  fromSubList _ blist@(length -> l) _ | l <= leafSize =
    KdLeaf (Just blist)
    
  fromSubList node_bbox bList axis = node
    where
      node =
        KdNode {
          kdLeft     = fromSubList left_node_bbox front (nextAxis axis)
        , nodeBBox   = node_bbox
        , kdSplit    = (axis, attrib_value axis split)
        , overlapped = overlaps axis split sortedBoxes
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

      -- | compute and map midpoint of each (BBox3,Int)
      --   for the sort
--    midpoints :: [(Vector3,(BBox3,a))]
      midpoints = map midpoint bList

      -- | Sorts based on splitting axis
--    sortedBoxes :: [(Vector3,(BBox3,a))]
      sortedBoxes = L.sortBy (sort_by_attrib axis) midpoints

      -- | Computes BBoxs overlapping split
      overlaps :: Axes BBox3            -> 
                  Vector3               -> 
                  [(Vector3,(BBox3,a))] -> 
                  [(BBox3, a)] 
      overlaps axis split sortedBoxes =
        mapMaybe (overlap axis split) sortedBoxes

      overlap :: Axes BBox3          -> 
                 Vector3             -> 
                 (Vector3,(BBox3,a)) -> 
                 Maybe (BBox3, a)
      overlap axis (Vector3 x y z) (_,(bbox,val)) = case axis of
        (X AxisX) -> bool Nothing (Just region) (R.within_bounds x x_range)
        (Y AxisY) -> bool Nothing (Just region) (R.within_bounds y y_range)
        (Z AxisZ) -> bool Nothing (Just region) (R.within_bounds z z_range)
        where
          region = (bbox,val) 
          x_range = axis_range AxisX bbox
          y_range = axis_range AxisY bbox
          z_range = axis_range AxisZ bbox

  sort_by_attrib :: Axes BBox3          -> 
                    (Vector3,(BBox3,a)) -> 
                    (Vector3,(BBox3,a)) -> 
                    Ordering
  sort_by_attrib axis p q =
    (attrib_value axis (fst p)) `compare` (attrib_value axis (fst q))

  midpoint :: (BBox3,a) -> (Vector3,(BBox3,a))
  midpoint bp@(b,_) = (midpoint',bp)
    where
      midpoint' = Vector3 minx miny minz
      (Vector3 minx miny minz) = min_point b
      (Vector3 maxx maxy maxz) = max_point b
      midx = (maxx + minx) / 2
      midy = (maxy + miny) / 2
      midz = (maxz + minz) /2

  attrib_value :: Axes BBox3 -> Vector3 -> Scalar
  attrib_value (X AxisX) vect = get_coord AxisX vect
  attrib_value (Y AxisY) vect = get_coord AxisY vect
  attrib_value (Z AxisZ) vect = get_coord AxisZ vect

-- | Utilities

-- | mergeResults
--   mergeResults favors Left collisions over Right nearest
mergeResults :: Nearest BBox3 a -> Nearest BBox3 a -> Nearest BBox3 a
mergeResults (Left (Collisions list1)) (Left (Collisions list2)) =
  Left (Collisions (list1 ++ list2))
mergeResults (Right (Just list1)) (Right (Just list2)) =
  Right (Just (list1 <> list2))
mergeResults list@(Left _) _ = list
mergeResults _ list@(Left _) = list

-- | nearest produces intermediate value for
--   
nearest :: [(Scalar, BBox3, a)] ->
           Either (Collisions BBox3 a) [(Scalar,BBox3,a)]
nearest dists = case (partitionDistances (map findDistances dists)) of
  Right candidates -> Right (filterCandidates $ L.sortBy distance candidates)
  collisions       -> collisions

-- | partitionDistances only returns non-collided if collided don't exist
partitionDistances :: [Either (BBox3,a) (Scalar,BBox3,a)] ->
                      Either (Collisions BBox3 a) [(Scalar,BBox3,a)]
partitionDistances e = case (partitionEithers e) of
  ([],distances) -> Right distances
  (collisions,_) -> Left (Collisions collisions)

-- | findDistances delianates collisions and non-collisions
findDistances :: (Scalar, BBox3, a) -> Either (BBox3,a) (Scalar,BBox3,a)
findDistances a@(dist,box,value)
  | dist == 0 = Left (box,value) -- collision!
  | otherwise = Right a

distance dist1@(d1,_,_) dist2@(d2,_,_) = d1 `compare` d2

-- | filterCandidates
--   returns all eqidistant nearest
filterCandidates :: [(Scalar,BBox3,a)] -> [(Scalar,BBox3,a)]
filterCandidates dists = filterCandidates' (L.sortBy distance dists)

filterCandidates' :: [(Scalar,BBox3,a)] -> [(Scalar,BBox3,a)]
filterCandidates' (v1@(dist1,_,_):v2@(dist2,_,_):xs)
  | dist1 == dist2 = v1:filterCandidates' (v2:xs)
  | otherwise      = v1:filterCandidates' []

-- | evaluateDistanceFromNode
--   evaluateDistanceFromNode compares distance of query box
--   to node bounding box
evaluateDistanceFromNode :: (Scalar, BBox3, a) -> BBox3 -> BBox3 -> Bool
evaluateDistanceFromNode  (dist1,_,_) bx nbx = dist1 < dist2
  where
    dist2 = sqrt ( (distx'^2) + (disty'^2) + (distz'^2) )
    distx' = boxAxisDistance (X AxisX) bx nbx
    disty' = boxAxisDistance (Y AxisY) bx nbx
    distz' = boxAxisDistance (Z AxisZ) bx nbx
    
