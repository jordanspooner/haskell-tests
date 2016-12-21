module LTS where

import Data.List
import Data.Maybe

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process]
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------
-- PART I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp x ps
  = fromJust (lookup x ps)

states :: LTS -> [State]
states lts
  = nub (concatMap (\((s1, s2), _) -> [s1, s2]) lts)

transitions :: State -> LTS -> [Transition]
transitions state
  = filter (\((s, _), _) -> s == state)

alphabet :: LTS -> Alphabet
alphabet
  = nub . map snd

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
actions (Prefix i p)
  = i : actions p
actions (Choice ps)
  = concatMap actions ps
actions _
  = []

accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts is ps@(p : _)
  = accepts' is ps (snd p)
    where
      accepts' :: [Id] -> [ProcessDef] -> Process -> Bool
      accepts' [] _ _
        = True
      accepts' is ps (Ref i')
        = accepts' is ps (lookUp i' ps)
      accepts' (i : is) ps (Prefix i' p')
        = i == i' && accepts' is ps p'
      accepts' is@(i : _) ps (Choice (p' : ps'))
        | accepts' [i] ps p' = accepts' is ps p'
        | otherwise          = accepts' is ps (Choice ps')
      accepts' _ _ _
        = False

------------------------------------------------------
-- PART III

--composeTransitions :: Transition -> Transition
--                   -> Alphabet -> Alphabet
--                   -> StateMap
--                   -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions :: Transition -> Transition -> Alphabet -> Alphabet
                      -> StateMap -> [Transition]
composeTransitions ((s, t), a) ((s', t') ,a') al1 al2 m
  | a == a'                       = [((ss', lookUp (t, t') m), a)]
  | a `elem` al2 && a' `elem` al1 = []
  | a' `elem` al1                 = [((ss', ts'), a)]
  | a `elem` al2                  = [((ss', st'), a')]
  | otherwise                     = [((ss', ts'), a), ((ss', st'), a')]
  where
    ss' = lookUp (s, s') m
    st' = lookUp (s, t') m
    ts' = lookUp (t, s') m

pruneTransitions :: [Transition] -> LTS
pruneTransitions ts
  = nub (visit 0 [])
    where
      visit :: State -> [State] -> [Transition]
      visit s ss
        | s `elem` ss = []
        | otherwise   = ts' ++ concatMap (flip visit (s : ss)) ss'
        where
          ts' = transitions s ts
          ss' = map (snd . fst) ts'

-- For testing
m :: StateMap
m = [((0,0),0),((0,1),1),((1,0),2),((1,1),3)]

------------------------------------------------------
-- PART IV

compose :: LTS -> LTS -> LTS
compose lts1 lts2
  = (pruneTransitions . concat)
    [composeTransitions t1 t2 a1 a2 (getStateMap s1 s2)
    | t1 <- lts1 ++ sentinels1 s1, t2 <- lts2 ++ sentinels2 s2]
      where
        a1         = "$2" : alphabet lts1
        a2         = "$1" : alphabet lts2
        s1         = states lts1
        s2         = states lts2
        sentinels1 = map (\s -> ((s, 0), "$1"))
        sentinels2 = map (\s -> ((s, 0), "$2"))

getStateMap :: [State] -> [State] -> StateMap
getStateMap ss1 ss2 = zip [(s1, s2) | s1 <- ss1, s2 <- ss2]
                          [0..length ss1 * length ss2 - 1]

------------------------------------------------------
-- PART V

buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS []
  = []
buildLTS pds
  = (\(_, _, lts) -> lts)
    (buildProcesses pds 0 (length pds) [] (zip (map fst pds) [0..]))

buildProcesses :: [ProcessDef] -> State -> State -> LTS -> [(Id, State)]
                  -> (State, State, LTS)
buildProcesses [] sNow sNext lts refs
  = (sNow, sNext, lts)
buildProcesses (pd : pds) sNow sNext lts refs
  = buildProcesses pds sNow' sNext' lts' refs
    where
      (sNow', sNext', lts') = buildProcess p (lookUp pid refs) sNext lts refs
      (pid, p)              = pd

buildProcess :: Process -> State -> State -> LTS -> [(Id, State)]
                -> (State, State, LTS)
buildProcess STOP sNow sNext lts refs
  = (sNow, sNext, lts)
buildProcess (Ref i) sNow sNext lts refs
  = (sNow', sNext', lts')
    where
      (sNow', sNext')    = (lookUp i refs, sNow)
      lts'               = map rename lts
      rename ((f, t), i) = if t == sNow then ((f, sNow'), i) else ((f, t), i)
buildProcess (Prefix i p) sNow sNext lts refs
  = buildProcess p sNext (sNext + 1) (((sNow, sNext), i) : lts) refs
buildProcess (Choice []) sNow sNext lts refs
  = (sNow, sNext, lts)
buildProcess (Choice (p : ps)) sNow sNext lts refs
  = buildProcess (Choice ps) sNow sNext' lts' refs
    where (_, sNext', lts') = buildProcess p sNow sNext lts refs

------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")),
                     Prefix "end" STOP])

maker
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch
  = ("SWITCH", Ref "OFF")

off
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS,
  pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS
  = [((0,1),"tick"),((1,0),"tock")]

playLTS
  = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS
  = [((0,1),"make"),((1,0),"ready")]

userLTS
  = [((0,1),"ready"),((1,0),"use")]

makerUserLTS
  = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS
  = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS
  = [((0,1),"on"),((1,0),"off")]
