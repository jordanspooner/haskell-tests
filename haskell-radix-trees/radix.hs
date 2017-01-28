module Radix where

data IntTree = EmptyTree | InternalNode IntTree Int IntTree
               deriving Show

buildIntTree :: [Int] -> IntTree
buildIntTree = foldr add EmptyTree

add :: Int -> IntTree -> IntTree
add x EmptyTree = InternalNode EmptyTree x EmptyTree
add x (InternalNode l y r)
  | x == y    = InternalNode l y r
  | x <= y    = InternalNode (add x l) y r
  | otherwise = InternalNode l y (add x r)

-- Simple random number generator...
a, m :: Integer
m = 1073741824
a = 16387

rand :: Integer -> [Float]
rand s
  = fromInteger s / fromInteger m : rand s' where s' = ( s * a ) `mod` m

randomInts :: Int -> Int -> Integer -> [Int]
randomInts m n s
  = take m (map (round . (+1) . (* fromIntegral n)) (rand s))

rs :: [Int]
rs = randomInts 1000 1000 816211275

rs1 :: [Int]
rs1 = randomInts 20 1000 816211275

--------------------------------------------------------------------------

data Colour = Black | White
              deriving (Eq, Ord, Show)

data RadixTree = Leaf Colour | Node Colour RadixTree RadixTree
                 deriving (Eq, Ord, Show)

type BitString = [Int]


-----------------------------------------------------------------------------
-- Some test trees...

figure :: RadixTree
figure = Node Black (Leaf White)
                    (Node White (Leaf Black)
                                (Node White (Node Black (Leaf White)
                                                        (Leaf Black))
                                            (Leaf White)))

tree1 :: IntTree
tree1 = InternalNode (InternalNode EmptyTree
                                   8
                                   (InternalNode EmptyTree 12 EmptyTree))
                     20
                     EmptyTree

tree2 :: RadixTree
tree2 = Node Black (Node Black (Leaf White)
                               (Node White (Leaf Black) (Leaf White)))
                   (Leaf White)

--------------------------------------------------------------------------

size :: IntTree -> Int
size (InternalNode t _ t') = 13 + size t + size t'
size EmptyTree             = 1

size' :: RadixTree -> Int
size' (Node _ t t') = 9 + size' t + size' t'
size' (Leaf _)      = 1

binary :: Int -> BitString
-- You can also use unfoldr here but there isn't much of a difference
binary 0 = [0]
binary x = reverse (binary' x)
  where
    binary' 0 = []
    binary' x' = remainder : binary' quotient
      where
        (quotient, remainder) = quotRem x' 2

insert :: BitString -> RadixTree -> RadixTree
insert [] (Node _ t t') = Node White t t'
insert [] (Leaf _) = Leaf White
insert (x:xs) (Node c t t')
  | x == 0 = Node c (insert xs t) t'
  | x == 1 = Node c t (insert xs t')
insert (x:xs) (Leaf c)
  | x == 0 = Node c (insert xs (Leaf Black)) (Leaf Black)
  | x == 1 = Node c (Leaf Black) (insert xs (Leaf Black))

buildRadixTree :: [Int]-> RadixTree
buildRadixTree
  = foldr (insert . binary) (Leaf Black)

member :: Int -> RadixTree -> Bool
-- This is not a very efficient implementation but follows the instructions
-- given on the test paper
member x t
  = t == insert (binary x) t

u :: Colour -> Colour -> Colour
-- Helper for union
u Black Black = Black
u _ _         = White

n :: Colour -> Colour -> Colour
-- Helper for intersection
n White White = White
n _ _         = Black

union :: RadixTree -> RadixTree -> RadixTree
union (Node c1 t1 t1') (Node c2 t2 t2')
  = Node (u c1 c2) (union t1 t2) (union t1' t2')
union (Node c1 t1 t1') (Leaf c2)
  = Node (u c1 c2) t1 t1'
union (Leaf c1) (Node c2 t2 t2')
  = Node (u c1 c2) t2 t2'
union (Leaf c1) (Leaf c2)
  = Leaf (u c1 c2)

intersection :: RadixTree -> RadixTree -> RadixTree
intersection t1 t2 = cleanUp (intersection' t1 t2)
  where
    -- Removes pairs of black leaves
    cleanUp :: RadixTree -> RadixTree
    cleanUp (Node c (Leaf Black) (Leaf Black)) = Leaf c
    cleanUp t                                  = t
    -- Finds (correct but possibly non-optimal) intersection
    intersection' :: RadixTree -> RadixTree -> RadixTree
    intersection' (Node c1 t1 t1') (Node c2 t2 t2')
      = Node (n c1 c2) (intersection t1 t2) (intersection t1' t2')
    intersection' (Node c1 _ _) (Leaf c2)
      = Leaf (n c1 c2)
    intersection' (Leaf c1) (Node c2 _ _)
      = Leaf (n c1 c2)
    intersection' (Leaf c1) (Leaf c2)
      = Leaf (n c1 c2)

radixBetter :: Int -> Bool
radixBetter n
  = size' (buildRadixTree ns) < size (buildIntTree ns)
    where
      ns = take n rs

radixValue :: Int
radixValue
  = head (filter radixBetter [1..])

-- Radix trees are preferred for more than 290 of the given pseudo-random
-- numbers
