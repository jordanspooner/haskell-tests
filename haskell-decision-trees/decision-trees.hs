import Data.Maybe
import Data.List

type AttName = String

type AttValue = String

type Attribute = (AttName, [AttValue])

type Header = [Attribute]

type Row = [AttValue]

type DataSet = (Header, [Row])

data DecisionTree = Null |
                    Leaf AttValue | 
                    Node AttName [(AttValue, DecisionTree)]
                  deriving (Eq, Show)

type Partition = [(AttValue, DataSet)]

type AttSelector = DataSet -> Attribute -> Attribute

xlogx :: Double -> Double
xlogx p
  | p <= 1e-100 = 0.0
  | otherwise   = p * log2 p 
  where
    log2 x = logBase 2 x

lookUp :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp x table
  = fromMaybe (error ("lookUp error - no binding for " ++ show x ++ 
                      " in table: " ++ show table))
              (lookup x table)

--------------------------------------------------------------------
-- PART I
--------------------------------------------------------------------

allSame :: Eq a => [a] -> Bool
allSame []
  = True
allSame (x:xs)
  = all (== x) xs

remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove x
  = filter ((/= x) . fst)

lookUpAtt :: AttName -> Header -> Row -> AttValue
--Pre: The attribute name is present in the given header.
lookUpAtt an ans avs
  = avs !! fromJust (elemIndex an (map fst ans))

removeAtt :: AttName -> Header -> Row -> Row
removeAtt an ans avs
  = avs \\ [lookUpAtt an ans avs]

addToMapping :: Eq a => (a, b) -> [(a, [b])] -> [(a, [b])]
addToMapping (x', v') []
  = [(x', [v'])]
addToMapping (x', v') (xvs@(x, vs) : xvss)
  | x == x'   = (x, v' : vs) : xvss
  | otherwise = xvs : addToMapping (x', v') xvss

buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
--Pre: Each row of the data set contains an instance of the attribute
buildFrequencyTable (an, avs) (h, rs)
  = [(av, (length . filter (== av)) vals) | av <- avs]
  where
    vals = map (lookUpAtt an h) rs

--------------------------------------------------------------------
-- PART II
--------------------------------------------------------------------

nodes :: DecisionTree -> Int
nodes (Node _ avdts)
  = 1 + sum (map (nodes . snd) avdts)
nodes (Leaf _)
  = 1
nodes Null
  = 0

evalTree :: DecisionTree -> Header -> Row -> AttValue
evalTree (Node an avdts) h rs
  = evalTree (lookUp av avdts) h rs
  where av = lookUpAtt an h rs
evalTree (Leaf av) _ _
  = av
evalTree Null _ _
  = ""

--------------------------------------------------------------------
-- PART III
--------------------------------------------------------------------

--
-- Given...
-- In this simple case, the attribute selected is the first input attribute 
-- in the header. Note that the classifier attribute may appear in any column,
-- so we must exclude it as a candidate.
--
nextAtt :: AttSelector
--Pre: The header contains at least one input attribute
nextAtt (header, _) (classifierName, _)
  = head (filter ((/= classifierName) . fst) header)

partitionData :: DataSet -> Attribute -> Partition
partitionData (h, rs) (an, avs)
  = [(an', (h', rs')) | (an', rs') <- anrs']
  where
    h'    = remove an h
    avs'  = map (lookUpAtt an h) rs
    rs'   = map (removeAtt an h) rs
    anrs' = foldr addToMapping (zip avs (repeat [])) (zip avs' rs')

buildTree :: DataSet -> Attribute -> AttSelector -> DecisionTree 
buildTree (h, []) _ _
  = Null
buildTree ds@(h, rs) att@(an, avs) selector
  | allSame vals
    = Leaf (head vals)
  | otherwise
    = Node an' [(av', buildTree ds' att selector) | (av', ds') <- p]
  where
    vals             = map (lookUpAtt an h) rs
    att'@(an', avs') = selector ds att
    p                = partitionData ds att'

--------------------------------------------------------------------
-- PART IV
--------------------------------------------------------------------

prob :: DataSet -> Attribute -> AttValue -> Double
prob ds@(_, rs) att av
  = fromIntegral (lookUp av freqs) / t
  where
    freqs = buildFrequencyTable att ds
    t     = (fromIntegral . length) rs

entropy :: DataSet -> Attribute -> Double
entropy (_, []) _
  = 0
entropy ds@(h, rs) att@(an, avs)
  = - foldr ((+) . xlogx . prob ds att) 0 avs

gain :: DataSet -> Attribute -> Attribute -> Double
gain ds@(h, rs) attp@(anp, avps) attc
  = entropy ds attc - sum (map reduce avps)
  where
    reduce avp = prob ds attp avp * entropy dsp attc
      where
        dsp = lookUp avp (partitionData ds attp)

bestGainAtt :: AttSelector
bestGainAtt ds@(h, _) att@(an, _)
  = h' !! (fromJust . elemIndex (maximum gains)) gains
  where
    h'    = remove an h
    gains = map (\attp -> gain ds attp att) h'

--------------------------------------------------------------------

outlook :: Attribute
outlook 
  = ("outlook", ["sunny", "overcast", "rainy"])

temp :: Attribute 
temp 
  = ("temp", ["hot", "mild", "cool"])

humidity :: Attribute 
humidity 
  = ("humidity", ["high", "normal"])

wind :: Attribute 
wind 
  = ("wind", ["windy", "calm"])

result :: Attribute 
result
  = ("result", ["good", "bad"])

fishingData :: DataSet
fishingData
  = (header, table)

header :: Header
table  :: [Row]
header 
  =  [outlook,    temp,   humidity, wind,    result] 
table 
  = [["sunny",    "hot",  "high",   "calm",  "bad" ],
     ["sunny",    "hot",  "high",   "windy", "bad" ],
     ["overcast", "hot",  "high",   "calm",  "good"],
     ["rainy",    "mild", "high",   "calm",  "good"],
     ["rainy",    "cool", "normal", "calm",  "good"],
     ["rainy",    "cool", "normal", "windy", "bad" ],
     ["overcast", "cool", "normal", "windy", "good"],
     ["sunny",    "mild", "high",   "calm",  "bad" ],
     ["sunny",    "cool", "normal", "calm",  "good"],
     ["rainy",    "mild", "normal", "calm",  "good"],
     ["sunny",    "mild", "normal", "windy", "good"],
     ["overcast", "mild", "high",   "windy", "good"],
     ["overcast", "hot",  "normal", "calm",  "good"],
     ["rainy",    "mild", "high",   "windy", "bad" ]]

--
-- This is the same as the above table, but with the result in the second 
-- column...
--
fishingData' :: DataSet
fishingData'
  = (header', table')

header' :: Header
table'  :: [Row]
header' 
  =  [outlook,    result, temp,   humidity, wind] 
table' 
  = [["sunny",    "bad",  "hot",  "high",   "calm"],
     ["sunny",    "bad",  "hot",  "high",   "windy"],
     ["overcast", "good", "hot",  "high",   "calm"],
     ["rainy",    "good", "mild", "high",   "calm"],
     ["rainy",    "good", "cool", "normal", "calm"],
     ["rainy",    "bad",  "cool", "normal", "windy"],
     ["overcast", "good", "cool", "normal", "windy"],
     ["sunny",    "bad",  "mild", "high",   "calm"],
     ["sunny",    "good", "cool", "normal", "calm"],
     ["rainy",    "good", "mild", "normal", "calm"],
     ["sunny",    "good", "mild", "normal", "windy"],
     ["overcast", "good", "mild", "high",   "windy"],
     ["overcast", "good", "hot",  "normal", "calm"],
     ["rainy",    "bad",  "mild", "high",   "windy"]]

fig1 :: DecisionTree
fig1
  = Node "outlook" 
         [("sunny", Node "temp" 
                         [("hot", Leaf "bad"),
                          ("mild",Node "humidity" 
                                       [("high",   Leaf "bad"),
                                        ("normal", Leaf "good")]),
                          ("cool", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "temp" 
                         [("hot", Null),
                          ("mild", Node "humidity" 
                                        [("high",Node "wind" 
                                                      [("windy",  Leaf "bad"),
                                                       ("calm", Leaf "good")]),
                                         ("normal", Leaf "good")]),
                          ("cool", Node "humidity" 
                                        [("high", Null),
                                         ("normal", Node "wind" 
                                                         [("windy",  Leaf "bad"),
                                                          ("calm", Leaf "good")])])])]

fig2 :: DecisionTree
fig2
  = Node "outlook" 
         [("sunny", Node "humidity" 
                         [("high", Leaf "bad"),
                          ("normal", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "wind" 
                         [("windy", Leaf "bad"),
                          ("calm", Leaf "good")])]


outlookPartition :: Partition
outlookPartition
  = [("sunny",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","bad"],["hot","high","windy","bad"],
                   ["mild","high","calm","bad"],["cool","normal","calm","good"],
                   ["mild","normal","windy","good"]])),
     ("overcast",([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","good"],["cool","normal","windy","good"],
                   ["mild","high","windy","good"],["hot","normal","calm","good"]])),
     ("rainy",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["mild","high","calm","good"],["cool","normal","calm","good"],
                   ["cool","normal","windy","bad"],["mild","normal","calm","good"],
                   ["mild","high","windy","bad"]]))]
