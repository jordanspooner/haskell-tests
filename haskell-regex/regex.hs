module Regex where

import Data.Maybe
import Data.List

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int

data Label = C Char | Eps
           deriving (Eq, Ord, Show)

type Transition = (State, State, Label)

type Automaton = (State, [State], [Transition])

--------------------------------------------------------
-- showRE - this may be useful for testing

showRE :: RE -> String
showRE (Seq re re')
  = showRE re ++ showRE re'
showRE (Alt re re')
  = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)
  = showRE' re ++ "*"
showRE (Plus re)
  = showRE' re ++ "+"
showRE (Opt re)
  =  showRE' re ++ "?"
showRE re
  = showRE' re

showRE' :: RE -> String
showRE' Null
  = ""
showRE' (Term c)
  = [c]
showRE' (Alt re re')
  = showRE (Alt re re')
showRE' re
  = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Part I

lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: There is exactly one occurrence of the item being looked up.
lookUp x xys
  = fromJust (lookup x xys)

simplify :: RE -> RE
simplify (Seq re1 re2)
  = Seq (simplify re1) (simplify re2)
simplify (Alt re1 re2)
  = Alt (simplify re1) (simplify re2)
simplify (Rep re)
  = Rep (simplify re)
simplify (Plus re)
  = Seq sre (Rep sre)
    where sre = simplify re
simplify (Opt re)
  = Alt (simplify re) Null
simplify re
  = re

--------------------------------------------------------
-- Part II

startState :: Automaton -> State
startState (s0, _, _)
  = s0
terminalStates :: Automaton -> [State]
terminalStates (_, sfs, _)
  = sfs
transitions :: Automaton -> [Transition]
transitions (_, _, ts)
  = ts

isTerminal :: State -> Automaton -> Bool
isTerminal s a
  = s `elem` terminalStates a

transitionsFrom :: State -> Automaton -> [Transition]
transitionsFrom s a
  = filter (\(f, _, _) -> f == s) (transitions a)

labels :: [Transition] -> [Label]
labels ts
  = nub (filter (/= Eps) (map (\(_, _, l) -> l) ts))

accepts :: Automaton -> String -> Bool
accepts a
  = accepts' (startState a)
    where
      accepts' :: State -> String -> Bool
      accepts' s cs
        | isTerminal s a && null cs
          = True
        | otherwise
          = any (try cs) (transitionsFrom s a)
      try :: String -> Transition -> Bool
      try cs (f, t, Eps)
        = accepts' t cs
      try [] (f, t, C d)
        = False
      try (c' : cs) (f, t, C c)
        | c' == c   = accepts' t cs
        | otherwise = False

--------------------------------------------------------
-- Part III

makeNDA :: RE -> Automaton
makeNDA re
  = (1, [2], sort transitions)
  where
    (transitions, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int)
make Null m n k
  = ([(m, n, Eps)], k)
make (Term c) m n k
  = ([(m, n, C c)], k)
make (Seq r1 r2) m n k
  = ((k, k + 1, Eps) : ts1 ++ ts2, sf2)
    where
      (ts1, sf1) = make r1 m k (k + 2)
      (ts2, sf2) = make r2 (k + 1) n sf1
make (Alt r1 r2) m n k
  = ([(m, k, Eps), (m, k + 2, Eps), (k + 1, n, Eps), (k + 3, n, Eps)]
      ++ ts1 ++ ts2, sf2)
    where
      (ts1, sf1) = make r1 k (k + 1) (k + 4)
      (ts2, sf2) = make r2 (k + 2) (k + 3) sf1
make (Rep r) m n k
  = ([(m, k, Eps), (k + 1, k, Eps), (k + 1, n, Eps), (m, n, Eps)] ++ ts, sf)
    where
      (ts, sf) = make r k (k + 1) (k + 2)

--------------------------------------------------------
-- Part IV

type MetaState      = [State]
type MetaTransition = (MetaState, MetaState, Label)

getFrontier :: State -> Automaton -> [Transition]
getFrontier s a
  = follow (transitionsFrom s a)
    where
      follow :: [Transition] -> [Transition]
      follow []
        | isTerminal s a = [(s, s, Eps)]
        | otherwise      = []
      follow ((f, t, l) : ts)
        | isTerminal t a && l == Eps = (t, t, Eps) : follow ts
        | l == Eps                   = follow (transitionsFrom t a ++ ts)
        | otherwise                  = (f, t, l) : follow ts

groupTransitions :: [Transition] -> [(Label, [State])]
groupTransitions ts
  = [(l, [t | (_, t, l') <- ts, l == l']) | l <- labels ts]

makeDA :: Automaton -> Automaton
-- Pre: Any cycle in the NDA must include at least one non-Eps transition
makeDA a@(s0, sfs, ts)
  = (1, finals, convertTransitions mts table)
    where
      (ms, mss, mts)
        = makeDA' [s0] [] []
      table
        = zip revmss [1..]
      finals
        = nub (map (flip lookUp table) (finalmss revmss sfs))
      revmss
        = reverse mss

      makeDA' :: [State] -> [MetaState] -> [MetaTransition]
                 -> (MetaState, [MetaState], [MetaTransition])
      makeDA' ss mss mts
        | ms `elem` mss = (ms, mss, mts)
        | otherwise     = extendDA' ms gts mss mts
          where
            ts  = concatMap (flip getFrontier a) ss
            ms  = metastate ts
            gts = groupTransitions ts

      extendDA' :: MetaState -> [(Label, [State])]
                   -> [MetaState] -> [MetaTransition]
                   -> (MetaState, [MetaState], [MetaTransition])
      extendDA' ms gts mss mts
        = foldl extend (ms, ms : mss, mts) gts
          where
            extend (ms, mss, mts) (label, ss)
              = (ms, mss', (ms, ms', label) : mts')
                where
                  (ms', mss', mts') = makeDA' ss mss mts

      metastate :: [Transition] -> [State]
      metastate ts
        = sort (nub (map (\(f, t, l) -> f) ts))

      convertTransitions :: [MetaTransition] -> [(MetaState, State)]
                            -> [Transition]
      convertTransitions mts tab
        = map (\(ms1, ms2, l) -> (lookUp ms1 tab, lookUp ms2 tab, l)) mts

      finalmss :: [MetaState] -> [State] -> [MetaState]
      finalmss mss
        = foldr (\ sf -> (++) (filter (\ ms -> sf `elem` ms) mss)) []

--------------------------------------------------------
-- Test cases

reFigure, re1, re2, re3, re4, re5 :: RE
reFigure
  = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1
  = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2
  = Seq (Term 'x') (Rep (Term '\''))
re3
  = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4
  = Seq (Alt (Term 'a') Null) (Term 'a')
re5
  = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))

nd, nd' :: Automaton
nd = (1,[4],[(1,2,C 'a'),(1,3,C 'b'),(2,3,Eps),(2,4,C 'c')])

nd' = (1,[4],[(1,2,Eps),(1,3,C 'a'),(2,4,C 'a'),(2,4,C 'b'),
              (3,4,C 'b'),(3,4,Eps)])

da :: Automaton
da = (0,[3],[(0,1,C 'a'),(0,2,C 'a'),(0,2,C 'b'),(1,2,C 'a'),
             (1,3,C 'b'),(2,2,C 'a'),(2,1,C 'a'),(2,3,C 'b')])

re :: RE
re = Seq (Alt (Term 'a') (Term 'b')) (Seq (Rep (Term 'a')) (Term 'b'))

ndaFigure, nda1, nda2, nda3, nda4, nda5 :: Automaton
daFigure, da1, da2, da3, da4, da5 :: Automaton
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],
     [(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1
  = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2
  = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2
  = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3
  = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3
  = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4
  = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5
  = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])
