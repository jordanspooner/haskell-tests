module Compression where

import Data.List
import Data.Char
import Data.Maybe

data HTree a = Leaf Int a | Node Int (HTree a) (HTree a)
               deriving (Show)

instance Eq (HTree a) where
  t1 == t2 = freqCount t1 == freqCount t2

instance Ord (HTree a) where
  t1 <= t2 = freqCount t1 <= freqCount t2

type Code = [Int]

freqCount :: HTree a -> Int
freqCount (Leaf n _)
  = n
freqCount (Node n _ _)
  = n

testString :: String
testString
  = "mississippi is missing"

fig :: HTree Char
fig
  = Node 22 (Node 8 (Node 4 (Leaf 2 'p') (Leaf 2 ' '))
                    (Node 4 (Node 2 (Leaf 1 'n') (Leaf 1 'g'))
                            (Leaf 2 'm')))
            (Node 14 (Leaf 7 'i') (Leaf 7 's'))

secretString :: String
secretString = decode code (rebuildTree tree)
  where
    code = [0,1,0,0,0,0,1,1,0,1,0,1,1,1,1,0,0,0,1,1,0,0,1,1,1,1,0,0]
    tree = [0,0,0,1,1,1,0,1,0,0,0,1,1,1,0,0,0,0,1,0,1,1,1,0,1,1,1,1,1,1,1,1,1,0,
            0,1,0,1,1,1,0,0,0,1,0,0,0,1,0,1,0,0,0,0,1,0,1,0,1,0,1,1,0,0,1,1,1,1,
            0,0,1,1,1,0,1,0,0,0,0,0]

--------------------------------------------------------------------------------

count :: Eq a => a -> [a] -> Int
count y xs
  = length (filter (== y) xs)

countAll :: Eq a => [a] -> [a] -> [(a, Int)]
countAll ys xs
  = map (\y -> (y, count y xs)) ys

buildTable :: Eq a => [a] -> [(a, Int)]
buildTable xs
  = countAll (nub xs) xs

merge :: HTree a -> HTree a -> HTree a
merge t1 t2
  = Node (freqCount t1 + freqCount t2) (min t1 t2) (max t1 t2)

reduce :: [HTree a] -> HTree a
-- Pre: The argument list is non-empty and sorted based on the ordering function
--      in the Ord instance above.
reduce [t]
  = t
reduce (t1 : t2 : ts)
  = reduce (insert (merge t1 t2) ts)

buildTree :: Eq a => [a] -> HTree a
-- Pre: The list is non-empty
buildTree xs
  = reduce (sort (map (\(x, n) -> Leaf n x) (buildTable xs)))

isElem :: Eq a => a -> HTree a -> Bool
isElem x (Node _ t1 t2)
  = isElem x t1 || isElem x t2
isElem x (Leaf _ y)
  = x == y

encode1 :: Eq a => a -> HTree a -> Code
encode1 _ (Leaf _ _)
  = []
encode1 x (Node _ t1 t2)
  | x `isElem` t1 = 0 : encode1 x t1
  | otherwise     = 1 : encode1 x t2

encode :: Eq a => [a] -> HTree a -> Code
-- Pre: The tree can encode each of the items in the list
encode xs t
  = concatMap (flip encode1 t) xs

decode :: Code -> HTree a -> [a]
-- Pre: The code is valid with respect to the tree
decode bs t
  = decode' bs t
    where
      decode' (b' : bs') (Node _ t1 t2)
        | b' == 0 = decode' bs' t1
        | b' == 1 = decode' bs' t2
      decode' [] (Leaf _ x)
        = [x]
      decode' bs' (Leaf _ x)
        = x : decode' bs' t

--------------------------------------------------------------------------------

maybeNextBit :: Int -> Maybe (Int, Int)
maybeNextBit 0
  = Nothing
maybeNextBit x
  = Just (remainder, quotient)
    where
      (quotient, remainder) = quotRem x 2

toBinary :: Int -> [Int]
toBinary 0
  = [0]
toBinary x
  = reverse (unfoldr maybeNextBit x)

fromBinary :: [Int] -> Int
fromBinary
  = foldl (\b1 b2 -> 2 * b1 + b2) 0

make7 :: [Int] -> [Int]
make7 xs
  = replicate (7 - length xs) 0 ++ xs

compressTree :: HTree Char -> [Int]
compressTree (Leaf _ x)
  = 1 : make7 (toBinary (ord x))
compressTree (Node _ t1 t2)
  = (0 : compressTree t1) ++ compressTree t2

rebuildTree :: [Int] -> HTree Char
-- Pre: The bitstring ([Int]) is a valid encoding of a Huffman Tree of
--      characters
rebuildTree (1 : bs)
  = Leaf 0 (chr (fromBinary bs))
rebuildTree (0 : bs)
  = mapFromJust (rebuildTree' bs (Node 0 (Leaf 0 Nothing) (Leaf 0 Nothing)))

rebuildTree' :: [Int] -> HTree (Maybe Char) -> HTree (Maybe Char)
rebuildTree' [] t
  = t
rebuildTree' (0 : bs) t
  = rebuildTree' bs (replaceNode t (Node 0 (Leaf 0 Nothing) (Leaf 0 Nothing)))
rebuildTree' (1 : bs) t
  = rebuildTree' rest (replaceNode t (Leaf 0 (Just (chr (fromBinary leaf)))))
    where
      (leaf, rest) = splitAt 7 bs

replaceNode :: HTree (Maybe Char) -> HTree (Maybe Char) -> HTree (Maybe Char)
replaceNode (Node _ t1 t2) replacement
  | containsNothing t1 = Node 0 (replaceNode t1 replacement) t2
  | otherwise          = Node 0 t1 (replaceNode t2 replacement)
replaceNode (Leaf _ Nothing) replacement
  = replacement

containsNothing :: HTree (Maybe Char) -> Bool
containsNothing (Node _ t1 t2)
  = containsNothing t1 || containsNothing t2
containsNothing (Leaf _ x)
  = isNothing x

mapFromJust :: HTree (Maybe a) -> HTree a
mapFromJust (Node _ t1 t2)
  = Node 0 (mapFromJust t1) (mapFromJust t2)
mapFromJust (Leaf _ x)
  = Leaf 0 (fromJust x)
