-- http://en.wikipedia.org/wiki/K-d_tree

module Data.Trees.KdTree where

import Data.Maybe

import qualified Data.Foldable as F
import qualified Data.List as L
import Test.QuickCheck

class Point p where
      -- |dimension returns the number of coordinates of a point.
      dimension :: p -> Int

      -- |coord gets the k'th coordinate, starting from 0.
      coord :: Int -> p -> Double

      -- |dist2 returns the squared distance between two points.
      dist2 :: p -> p -> Double
      dist2 a b = sum . map diff2 $ [0..dimension a - 1]
        where diff2 i = (coord i a - coord i b)^2

-- |compareDistance p a b  compares the distances of a and b to p.
compareDistance :: (Point p) => p -> p -> p -> Ordering
compareDistance p a b = dist2 p a `compare` dist2 p b

data Point3d = Point3d { p3x :: Double, p3y :: Double, p3z :: Double }
    deriving (Eq, Ord, Show)

instance Point Point3d where
    dimension _ = 3

    coord 0 p = p3x p
    coord 1 p = p3y p
    coord 2 p = p3z p


data KdTree point = KdNode { kdLeft :: KdTree point,
                             kdPoint :: point,
                             kdRight :: KdTree point,
                             kdAxis :: Int }
                  | KdEmpty
     deriving (Eq, Ord, Show)

instance Functor KdTree where
    fmap _ KdEmpty = KdEmpty
    fmap f (KdNode l x r axis) = KdNode (fmap f l) (f x) (fmap f r) axis

instance F.Foldable KdTree where
    foldr f init KdEmpty = init
    foldr f init (KdNode l x r _) = F.foldr f init3 l
        where init3 = f x init2
              init2 = F.foldr f init r

fromList :: Point p => [p] -> KdTree p
fromList points = fromListWithDepth points 0

-- |fromListWithDepth selects an axis based on depth so that the axis cycles
-- through all valid values.
fromListWithDepth :: Point p => [p] -> Int -> KdTree p
fromListWithDepth [] _ = KdEmpty
fromListWithDepth points depth = node
    where axis = depth `mod` dimension (head points) 

          -- Sort point list and choose median as pivot element
          sortedPoints =
              L.sortBy (\a b -> coord axis a `compare` coord axis b) points
          medianIndex = length sortedPoints `div` 2
        
          -- Create node and construct subtrees
          node = KdNode { kdLeft = fromListWithDepth (take medianIndex sortedPoints) (depth+1),
                          kdPoint = sortedPoints !! medianIndex,
                          kdRight = fromListWithDepth (drop (medianIndex+1) sortedPoints) (depth+1),
                          kdAxis = axis }

toList :: KdTree p -> [p]
toList t = F.foldr (:) [] t

-- |subtrees t returns a list containing t and all its subtrees, including the
-- empty leaf nodes.
subtrees :: KdTree p -> [KdTree p]
subtrees KdEmpty = [KdEmpty]
subtrees t@(KdNode l x r axis) = subtrees l ++ [t] ++ subtrees r

-- |nearestNeighbor tree p returns the nearest neighbor of p in tree.
nearestNeighbor :: Point p => KdTree p -> p -> Maybe p
nearestNeighbor KdEmpty probe = Nothing
nearestNeighbor (KdNode KdEmpty p KdEmpty _) probe = Just p
nearestNeighbor (KdNode l p r axis) probe =
    if xProbe <= xp then findNearest l r else findNearest r l
    where xProbe = coord axis probe
          xp = coord axis p
          findNearest tree1 tree2 =
                let candidates1 = case nearestNeighbor tree1 probe of
                                    Nothing -> [p]
                                    Just best1 -> [best1, p]
                    sphereIntersectsPlane = (xProbe - xp)^2 <= dist2 probe p
                    candidates2 = if sphereIntersectsPlane
                                    then candidates1 ++ maybeToList (nearestNeighbor tree2 probe)
                                    else candidates1 in
                Just . L.minimumBy (compareDistance probe) $ candidates2

  -- |nearNeighbors tree p returns all neighbors within distance r from p in tree.
nearNeighbors :: Point p => KdTree p -> Double -> p -> [p]
nearNeighbors KdEmpty radius probe                      = []
nearNeighbors (KdNode KdEmpty p KdEmpty _) radius probe = if dist2 p probe <= radius^2 then [p] else []
nearNeighbors (KdNode l p r axis) radius probe          =
    if xProbe <= xp
      then let nearest = maybePivot ++ nearNeighbors l radius probe
           in if xProbe + abs radius > xp
                then nearNeighbors r radius probe ++ nearest
                else nearest
      else let nearest = maybePivot ++ nearNeighbors r radius probe
           in if xProbe - abs radius < xp
                then nearNeighbors l radius probe ++ nearest
                else nearest
  where xProbe     = coord axis probe
        xp         = coord axis p
        maybePivot = if dist2 probe p <= radius^2 then [p] else []

-- |isValid tells whether the K-D tree property holds for a given tree.
-- Specifically, it tests that all points in the left subtree lie to the left
-- of the plane, p is on the plane, and all points in the right subtree lie to
-- the right.
isValid :: Point p => KdTree p -> Bool
isValid KdEmpty = True
isValid (KdNode l p r axis) = leftIsGood && rightIsGood
    where x = coord axis p
          leftIsGood = all ((<= x) . coord axis) (toList l)
          rightIsGood = all ((>= x) . coord axis) (toList r)

-- |allSubtreesAreValid tells whether the K-D tree property holds for the given
-- tree and all subtrees.
allSubtreesAreValid :: Point p => KdTree p -> Bool
allSubtreesAreValid = all isValid . subtrees

-- |kNearestNeighbors tree k p returns the k closest points to p within tree.
kNearestNeighbors :: (Eq p, Point p) => KdTree p -> Int -> p -> [p]
kNearestNeighbors KdEmpty _ _ = []
kNearestNeighbors _ k _ | k <= 0 = []
kNearestNeighbors tree k probe = nearest : kNearestNeighbors tree' (k-1) probe
    where nearest = fromJust $ nearestNeighbor tree probe
          tree' = tree `remove` nearest

-- |remove t p removes the point p from t.
remove :: (Eq p, Point p) => KdTree p -> p -> KdTree p
remove KdEmpty _ = KdEmpty
remove (KdNode l p r axis) pKill =
    if p == pKill
        then fromListWithDepth (toList l ++ toList r) axis
        else if coord axis pKill <= coord axis p
                then KdNode (remove l pKill) p r axis
                else KdNode l p (remove r pKill) axis

instance Arbitrary Point3d where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Point3d x y z)

