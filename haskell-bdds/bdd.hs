module BDD where

import Data.Maybe
import Data.List

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------
-- PART I

-- Pre: The item is in the given table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp a ps
  = fromJust (lookup a ps)

checkSat :: BDD -> Env -> Bool
checkSat (n, ns) env
  | n == 0       = False
  | n == 1       = True
  | lookUp i env = checkSat (r, ns) env
  | otherwise    = checkSat (l, ns) env
  where
    (i, l, r) = lookUp n ns

sat :: BDD -> [[(Index, Bool)]]
sat (n, ns)
  | n == 0    = []
  | n == 1    = [[]]
  | otherwise = satFalse ++ satTrue
  where
    (i, l, r) = lookUp n ns
    satFalse  = map ((i, False) :) (sat (l, ns))
    satTrue   = map ((i, True) :) (sat (r, ns))

------------------------------------------------------
-- PART II

simplify :: BExp -> BExp
simplify (Not (Prim b))
  = Prim (not b)
simplify (And (Prim b1) (Prim b2))
  = Prim (b1 && b2)
simplify (Or (Prim b1) (Prim b2))
  = Prim (b1 || b2)
simplify x
  = x

restrict :: BExp -> Index -> Bool -> BExp
restrict (IdRef j) i b
  | i == j    = Prim b
  | otherwise = IdRef j
restrict (Not x) i b
    = simplify (Not (restrict x i b))
restrict (And x1 x2) i b
    = simplify (And (restrict x1 i b) (restrict x2 i b))
restrict (Or x1 x2) i b
    = simplify (Or (restrict x1 i b) (restrict x2 i b))
restrict (Prim b) _ _
  = Prim b

------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
buildBDD :: BExp -> [Index] -> BDD
buildBDD e
  = buildBDD' e 2

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' e _ []
  | e == Prim False = (0, [])
  | otherwise       = (1, [])
buildBDD' e n (x:xs)
  = (n, (n, (x, lRoot, rRoot)) : lNodes ++ rNodes)
  where
    (lRoot, lNodes) = buildBDD' (restrict e x False) (2 * n) xs
    (rRoot, rNodes) = buildBDD' (restrict e x True) (2 * n + 1) xs

------------------------------------------------------
-- PART IV

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD e xs
  = (n, nub ns)
    where
      (n, ns) = buildROBDD' e 2 xs (getROBDDs e 2 xs)

buildROBDD' :: BExp -> NodeId -> [Index] -> [(BDD, NodeId)] -> BDD
buildROBDD' e _ [] _
  | e == Prim False = (0,[])
  | otherwise       = (1,[])
buildROBDD' e n (x:xs) ns
  | lROBDD == rROBDD = buildROBDD' lExp n xs ns
  | otherwise        = (n, (n, (x, lNode, rNode)) : lNodes ++ rNodes)
  where
    (lExp, rExp)     = (restrict e x False, restrict e x True)
    (lROBDD, rROBDD) = (buildROBDD lExp xs, buildROBDD rExp xs)
    (lNode, lNodes)  = (lookUp lROBDD ns, snd (buildROBDD' lExp lNode xs ns))
    (rNode, rNodes)  = (lookUp rROBDD ns, snd (buildROBDD' rExp rNode xs ns))

getROBDDs :: BExp -> NodeId -> [Index] -> [(BDD, NodeId)]
getROBDDs _ _ []
  = [((0, []), 0), ((1, []), 1)]
getROBDDs e n xs'@(x:xs)
  | lROBDD == rROBDD = getROBDDs lExp n xs
  | otherwise        = (buildROBDD e xs', n) : lROBDDs ++ rROBDDs
  where
    (lExp, rExp)     = (restrict e x False, restrict e x True)
    (lROBDD, rROBDD) = (buildROBDD lExp xs, buildROBDD rExp xs)
    lROBDDs          = getROBDDs lExp (2 * n) xs
    rROBDDs          = getROBDDs rExp (2 * n + 1) xs

------------------------------------------------------
-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))
b9 = And (Or (IdRef 1) (Not (IdRef 1))) (Or (IdRef 2) (Not (IdRef 2)))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])
