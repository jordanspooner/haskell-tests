module KMeans where

type Point = (Int, Int)

type Centroid = Point

type Assignment = (Point, Int)

type Clustering = ([Assignment], [Centroid])

distance :: Point -> Point -> Int
distance (x, y) (x', y')
  = abs (x' -x) + abs (y' - y)

sumPoints :: Point -> Point -> Point
sumPoints (x, y) (x', y')
  = (x + x', y + y')

centroid :: [Point] -> Centroid
centroid ps
  = normalise (foldl sumPoints (0,0) ps)
    where
      normalise (x, y) = (x `div` n, y `div` n)
      n = length ps

index :: Int -> [(Point, Int)] -> [Point]
index i ps
  = map fst (filter (\x -> snd x == i) ps)

assign :: Point -> [Centroid] -> Assignment
assign p cs
  = snd (minimum [(distance p c, (p, i)) | (i, c) <- indexedCs])
    where
      indexedCs = zip [1..] cs

assignAll :: [Point] -> [Centroid] -> [Assignment]
assignAll ps cs
  = map (`assign` cs) ps

findCentroids :: [Assignment] -> Int -> [Centroid]
findCentroids ass k
  = map (centroid . (`index` ass)) [1 .. k]

doIteration :: [Point] -> [Centroid] -> Int -> Clustering
doIteration ps cs k
  = (assignments, findCentroids assignments k)
    where
      assignments = assignAll ps cs

converge :: Eq a => [a] -> a
converge (x : ys @ (y : _))
  | x == y = x
  | otherwise = converge ys

cluster :: [Point] -> Int -> Clustering
cluster ps k = converge (iterate oneIteration (assignAll ps cs, cs))
  where
    cs = take k ps
    oneIteration (ps', cs') = doIteration (map fst ps') cs' k

ps1 :: [Point]
ps1 = [(2,3),(3,5),(6,7),(4,8),(9,3),(1,1),(2,1),(9,1),(7,2),(5,6),(5,9)]

figure1 :: Clustering
figure1
  = ([((2,3),1),((3,5),1),((6,7),3),((4,8),2),((9,3),3),((1,1),1),
      ((2,1),1),((9,1),1),((7,2),1),((5,6),3),((5,9),2)],
     [(4,2),(4,8),(6,5)])

-- Result of cluster ps1 3...
result1 :: Clustering
result1
 = ([((2,3),2),((3,5),2),((6,7),3),((4,8),3),((9,3),1),((1,1),1),
     ((2,1),1),((9,1),1),((7,2),1),((5,6),3),((5,9),3)],
    [(5,1),(2,4),(5,7)])

assignment1 :: [Assignment]
assignment1
  = fst result1

ps2 :: [Point]
ps2
  = [(10,10),(50,70),(15,20),(30,40),(35,50),(45,50),(35,45)]

-- Result of cluster ps2 2...
result2 :: Clustering
result2
 = ([((10,10),1),((50,70),2),((15,20),1),((30,40),2),((35,50),2),
     ((45,50),2),((35,45),2)],
    [(12,15),(39,51)])

assignment2 :: [Assignment]
assignment2
  = fst result2

ps3 :: [Point]
ps3
 = [(259,307), (198,308), (354,156), (388,166), (268,324), (296,332),
    (308,178), (286,195), (368,231), (276,284), (292,334), (141,152),
    (337,333), (316,106), (365,112), (345,186), (286,317), (280,132),
    (353,184), (378,224), (359,234), (155,234), (208,232), (390,182),
    (274,301), (303,306), (316,246), (365,234), (371,129), (43,289)]

-- Result of cluster ps3 4...
result3 :: Clustering
result3
 = ([((259,307),1),((198,308),1),((354,156),3),((388,166),4),((268,324),1),
     ((296,332),1),((308,178),3),((286,195),3),((368,231),4),((276,284),1),
     ((292,334),1),((141,152),2),((337,333),1),((316,106),3),((365,112),3),
     ((345,186),4),((286,317),1),((280,132),3),((353,184),4),((378,224),4),
     ((359,234),4),((155,234),2),((208,232),2),((390,182),4),((274,301),1),
     ((303,306),1),((316,246),4),((365,234),4),((371,129),3),((43,289),2)],
    [(278,314),(136,226),(325,144),(362,209)])

assignment3 :: [Assignment]
assignment3
  = fst result3
