module Dijkstra where

import Data.List
import Data.Maybe

type Id = Int

type Weight = Int

type Edge = (Id, Id)

type Graph = [(Edge, Weight)]

-- Why not just use the provided Maybe Int type for Cost???
data Cost = Finite Weight | Infinity
            deriving (Eq, Ord, Show)

instance Num Cost where
  (+) = addCosts

type PathCost = (Cost, Id)

g1 :: Graph
g1 = [((0, 1), 1),
      ((0, 2), 3),
      ((0, 4), 6),
      ((1, 2), 1),
      ((1, 3), 3),
      ((2, 0), 1),
      ((2, 1), 2),
      ((2, 3), 1),
      ((3, 0), 3),
      ((3, 4), 2),
      ((4, 3), 1),
      ((5, 2), 9)]

totalWeight :: Graph -> Weight
totalWeight
  = sum . map snd

edges :: Id -> Graph -> [Id]
edges node graph
  = [b | ((a, b), _) <- graph, a == node]

nodes :: Graph -> [Id]
nodes
  = nub . foldr ((\ (a, b) ns -> a : b : ns) . fst) []

addCosts :: Cost -> Cost -> Cost
addCosts (Finite x) (Finite y)
  = Finite (x + y)
addCosts _ _
  = Infinity

lookUp :: Edge -> Graph -> Cost
lookUp edge graph
  | isNothing maybeEdge = Infinity
  | otherwise           = Finite (fromJust maybeEdge)
    where maybeEdge = lookup edge graph

remove :: Eq a => a -> [a] -> [a]
remove
  = delete

allPaths :: Graph -> [PathCost]
allPaths graph
  = dijk (zip (map (\n -> lookUp (0,n) graph) ns) ns)
    where
      ns = nodes graph
      dijk :: [PathCost] -> [PathCost]
      dijk [] = []
      dijk ps = minp : dijk (map relax (remove minp ps))
        where
          minp @ (cj, j) = minimum ps
          relax :: PathCost -> PathCost
          relax (ci, i) = (minimum [ci, cj + lookUp (j, i) graph], i)
