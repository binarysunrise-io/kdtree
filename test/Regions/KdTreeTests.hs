module Regions.KdTreeTests 
  (prop_constructionProducesValidTrees) where

import qualified Data.Trees.KdTree.Regions.KThree.KThreeTree as Kd
import Data.Trees.KdTree.Regions.Internal (BBoxOffset)
import Regions.Internal
import Data.BoundingBox.B3

prop_constructionProducesValidTrees :: [(Kd.Vect BBox3,Int)] -> Bool
prop_constructionProducesValidTrees vects =
  allSubtreesAreValid . Kd.fromList $ boxes
  where
    boxes = Kd.toBBox (map boxInput vects) 



boxInput :: (Show a) => (Kd.Vect BBox3,a) -> (Kd.Vect BBox3, BBoxOffset, a)
boxInput (bbox3,a) = (bbox3,offset,a)

offset :: BBoxOffset
offset = 5.0
  
  
