module Suffix where

import Data.List hiding (insert, partition)

data SuffixTree = Leaf Int | Node [(String, SuffixTree)]
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix
  = isPrefixOf

-- isPrefix :: String -> String -> Bool
-- isPrefix s s'
--   = take (length s) s' == s

--Pre: s is a prefix of s'
removePrefix :: String -> String -> String
removePrefix s
  = drop (length s)

suffixes :: [a] -> [[a]]
suffixes
  = init . tails

-- suffixes :: [a] -> [[a]]
-- suffixes
--   = init (scanr (:) [])

isSubstring :: String -> String -> Bool
isSubstring
  = isInfixOf

-- isSubstring :: String -> String -> Bool
-- isSubstring s s'
--   = any (isPrefix s) (suffixes s')

findSubstrings :: String -> String -> [Int]
findSubstrings s s'
  = findIndices (isPrefix s) (suffixes s')

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf x)
  = [x]
getIndices (Node xs)
  = concatMap (getIndices . snd) xs

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition (x : xs) (y : ys)
  | x == y = (x : as, bs, cs)
  where
    (as, bs, cs) = partition xs ys
partition xs ys
  = ([], xs, ys)

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' "" (Leaf x)
  = [x]
findSubstrings' s (Node ((s', t') : ps))
  | null remS  = getIndices t'
  | null remS' = findSubstrings' remS t'
  | otherwise  = findSubstrings' s (Node ps)
  where
    (_, remS, remS') = partition s s'
findSubstrings' _ _
  = []

------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s, n) (Node [])
  = Node [(s, Leaf n)]
insert (s, n) (Node ((a, t) : ats))
  | null pre  = merge (Node [(a, t)]) (insert (s, n) (Node ats))
  | pre == a  = Node ((a, insert (remS, n) t) : ats)
  | otherwise = merge (Node [(pre, Node [(remS, Leaf n), (remA, t)])]) (Node ats)
  where
    (pre, remS, remA) = partition s a

-- Pre: Arguments must be nodes
merge :: SuffixTree -> SuffixTree -> SuffixTree
merge (Node xs) (Node ys)
  = Node (xs ++ ys)

-- This function is given
buildTree :: String -> SuffixTree
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..length s-1])

------------------------------------------------------
-- Part IV

-- Pre: Argument must be node
isRepeated :: SuffixTree -> Bool
isRepeated (Node xs)
  | length xs > 1 = True
  | otherwise     = False

-- Need to check lengths
allRepeatedSubstrings :: SuffixTree -> [String]
allRepeatedSubstrings (Leaf _)
  = []
allRepeatedSubstrings (Node [])
  = [[]]
allRepeatedSubstrings (Node ((s, n@(Node _)) : ps))
  | isRepeated n = map (s ++) (allRepeatedSubstrings n) ++ allRepeatedSubstrings (Node ps)
  | otherwise    = []
allRepeatedSubstrings (Node ((_, Leaf _) : ps))
  = allRepeatedSubstrings (Node ps)

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring t
  = head (sortOn (negate.length) (allRepeatedSubstrings t))

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1
  = "banana"

s2 :: String
s2
  = "mississippi"

t1 :: SuffixTree
t1
  = Node [("banana", Leaf 0),
          ("a", Node [("na", Node [("na", Leaf 1),
                                   ("", Leaf 3)]),
                     ("", Leaf 5)]),
          ("na", Node [("na", Leaf 2),
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2
  = Node [("mississippi", Leaf 0),
          ("i", Node [("ssi", Node [("ssippi", Leaf 1),
                                    ("ppi", Leaf 4)]),
                      ("ppi", Leaf 7),
                      ("", Leaf 10)]),
          ("s", Node [("si", Node [("ssippi", Leaf 2),
                                   ("ppi", Leaf 5)]),
                      ("i", Node [("ssippi", Leaf 3),
                                  ("ppi", Leaf 6)])]),
          ("p", Node [("pi", Leaf 8),
                      ("i", Leaf 9)])]
