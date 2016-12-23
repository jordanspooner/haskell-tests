module BinHeaps where

import Data.List hiding (insert)

type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

value :: BinTree a -> a
value (Node x _ _)
  = x

rank :: BinTree a -> Int
rank (Node _ r _)
  = r

children :: BinTree a -> [BinTree a]
children (Node _ _ ts)
  = ts

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees t t'
  | value t < value t' = Node (value t)  (rank t + 1)  (t' : children t)
  | otherwise          = Node (value t') (rank t' + 1) (t : children t')

--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
extractMin
  = minimum . map value

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps h []
  = h
mergeHeaps [] h'
  = h'
mergeHeaps h @ (t : ts) h' @ (t' : ts')
  | rank t < rank t' = t  : mergeHeaps ts  h'
  | rank t' < rank t = t' : mergeHeaps ts' h
  | otherwise        = mergeHeaps [combineTrees t t'] (mergeHeaps ts ts')

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert x
  = mergeHeaps [Node x 0 []]

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin h
  = mergeHeaps (reverse . children $ minTree h) (delete (minTree h) h)
  where
    minVal
      = extractMin h
    minTree (t : ts)
      | value t == minVal = t
      | otherwise         = minTree ts

makeHeap :: Ord a => [a] -> BinHeap a
makeHeap
  = foldr (mergeHeaps . flip insert []) []

sortHeap :: Ord a => BinHeap a -> [a]
sortHeap
  = unfoldr maybeMin
  where
    maybeMin h'
      | null h'   = Nothing
      | otherwise = Just (extractMin h', deleteMin h')

binSort :: Ord a => [a] -> [a]
binSort
  = sortHeap . makeHeap

--------------------------------------------------------------
-- PART III

toBinary :: BinHeap a -> [Int]
toBinary [] = []
toBinary ts
  = toBinary' ts 0 []
  where
    toBinary' [] _ acc
      = acc
    toBinary' ts'' @ (t' : ts') n acc
      | rank t' > n = toBinary' ts'' (n + 1) (0 : acc)
      | otherwise   = toBinary' ts'  (n + 1) (1 : acc)

binarySum :: [Int] -> [Int] -> [Int]
-- Folds result from binarySum' to produce an actual binary number
binarySum bs bs'
  | bit == 0 = bits
  | bit == 1 = 1 : bits
  where (bit, bits) = mapAccumR carry 0 (binarySum' bs bs')

carry :: Int -> Int -> (Int, Int)
carry b b'
  | b + b' == 0 = (0, 0)
  | b + b' == 1 = (0, 1)
  | b + b' == 2 = (1, 0)
  | b + b' == 3 = (1, 1)

binarySum' :: [Int] -> [Int] -> [Int]
-- Add element-wise two binary numbers. Result may contain '2's
binarySum' bs bs'
  = uncurry (zipWith (+)) (updateListLength bs bs')

updateListLength :: [Int] -> [Int] -> ([Int], [Int])
-- Make two binary numbers the same length by adding 0s to the beginning
-- of the shorter one
updateListLength l l'
  | lengthDif > 0 = (replicate lengthDif 0 ++ l, l')
  | otherwise     = (l, replicate (abs lengthDif) 0 ++ l')
  where lengthDif = length l' - length l

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []],
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []],
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []],
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]
