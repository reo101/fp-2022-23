-------------
----- 1 -----
-------------

type Team = String

type Points = Integer

type Match = (Team, Team, Points, Points)

type Tournament = [Match]

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
fab &&& fac = \a -> (fab a, fac a)

bimap :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
bimap fab fcd (a, c) = (fab a, fcd c)

totalGoals :: Tournament -> Team -> (Points, Points)
totalGoals [] _ = (0, 0)
totalGoals ((team1, team2, goals1, goals2) : rest) team
  | team1 == team = bimap (+ goals1) (`subtract` goals2) $ totalGoals rest team
  | team2 == team = bimap (+ goals2) (`subtract` goals1) $ totalGoals rest team
  | otherwise = totalGoals rest team

pointsForResult :: (Points, Points) -> Points
pointsForResult (goals1, goals2) =
  case goals1 `compare` goals2 of
    LT -> 0
    EQ -> 1
    GT -> 3

unique :: (Eq a) => [a] -> [a]
unique = foldl (\res x -> if x `elem` res then res else x : res) []

totalPoints :: Tournament -> Team -> Points
totalPoints [] _ = 0
totalPoints ((team1, team2, goals1, goals2) : rest) team
  | team1 == team = pointsForResult (goals1, goals2) + totalPoints rest team
  | team2 == team = pointsForResult (goals2, goals1) + totalPoints rest team
  | otherwise = totalPoints rest team

allTeams :: Tournament -> [Team]
allTeams tournament = unique $ concatMap (\(team1, team2, _, _) -> [team1, team2]) tournament

teamsWithMostPoints :: Tournament -> [Team]
teamsWithMostPoints tournament =
  filter isWithMaxPoints teams
  where
    teams :: [Team]
    teams = allTeams tournament

    pointsForTeam :: Team -> Points
    pointsForTeam = totalPoints tournament

    maxPoints :: Points
    maxPoints = maximum $ pointsForTeam <$> teams

    isWithMaxPoints :: Team -> Bool
    isWithMaxPoints team = pointsForTeam team == maxPoints

-- `Ord a` because of equalities of `fab a`
minimumBy :: (Ord a, Ord b) => (a -> b) -> [a] -> a
minimumBy fab xs = snd $ minimum $ (fab &&& id) <$> xs

diag :: (a -> a -> b) -> a -> b
diag f a = f a a

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe x Nothing = x

won :: Team -> Team -> Match -> Maybe Bool
won team1 team2 (team1', team2', goals1, goals2)
  | (team1 == team1') && (team2 == team2') = Just $ goals1 > goals2
  | (team1 == team2') && (team2 == team1') = Just $ goals2 > goals1
  | otherwise = Nothing

exceeds :: Tournament -> Team -> Team -> Bool
exceeds tournament team1 team2 = directlyBetter || indirectlyBetter
  where
    directlyBetter :: Bool
    directlyBetter = any (fromMaybe False . won team1 team2) tournament

    indirectlyBetter :: Bool
    indirectlyBetter = any isSuitableMiddleMan $ allTeams tournament
      where
        isSuitableMiddleMan :: Team -> Bool
        isSuitableMiddleMan team3 =
          team3 /= team1
            && team3 /= team2
            && all (fromMaybe True . won team1 team3) tournament
            && exceeds tournament team3 team2

---

t1 :: Tournament
t1 =
  [ ("A", "B", 1, 0),
    ("B", "C", 4, 1),
    ("B", "C", 0, 4),
    ("B", "A", 1, 2),
    ("A", "C", 0, 1)
  ]

-- >>> maxPointsMinGoal t1
-- "A"

maxPointsMinGoal :: Tournament -> Team
maxPointsMinGoal tournament =
  minimumBy (totalPoints tournament) (teamsWithMostPoints tournament)

-- >>> exceedSelf t1
-- ["C"]

exceedSelf :: Tournament -> [Team]
exceedSelf tournament = filter (diag $ exceeds tournament) (allTeams tournament)

-------------
----- 2 -----
-------------

data BinTree a = Leaf | Node (BinTree a) a (BinTree a)
  deriving (Show)

data Direction = ToLeft | ToRight
  deriving (Eq, Show)

type Trace = [Direction]

type Predicate a = a -> Bool

type PredicateTree a = BinTree (Predicate a)

trace :: PredicateTree a -> a -> Trace
trace Leaf _ = []
trace (Node left p right) x =
  if p x
    then ToRight : trace right x
    else ToLeft : trace left x

-- >>> prime <$> [1..10]
-- [False,True,True,False,True,False,True,False,False,False]

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
fbbc `on` fab = \a1 a2 -> fab a1 `fbbc` fab a2

prime :: Integer -> Bool
prime x = (== 2) $ length $ filter ((== 0) . (x `mod`)) [1 .. x]

-- >>> sameTraceWith t 2 4
-- True

sameTraceWith :: PredicateTree a -> a -> a -> Bool
sameTraceWith tree = (==) `on` trace tree

-- >>> diffPairs [1..4]
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

diffPairs :: [a] -> [(a, a)]
diffPairs [] = []
diffPairs (x : xs) = ((,) x <$> xs) ++ diffPairs xs

---

t2 :: PredicateTree Integer
t2 =
  Node (Node (Node Leaf
                   (>6)
                   Leaf)
             odd
             (Node Leaf
                   prime
                   Leaf))
       (<2)
       (Node Leaf
             (>0)
             Leaf)

-- >>> sameTrace t2 [1, 3, 4]
-- False

-- >>> sameTrace t2 [1, 3, 4, 9, 2]
-- True

sameTrace :: PredicateTree a -> [a] -> Bool
sameTrace tree xs = any (uncurry (sameTraceWith tree)) $ diffPairs xs

-------------
----- 3 -----
-------------

allPerms :: (a, a, a) -> [(a, a, a)]
allPerms (x, y, z) =
  [ (x, y, z),
    (x, z, y),
    (y, x, z),
    (y, z, x),
    (z, x, y),
    (z, y, x)
  ]

diff :: (Ord a, Num a) => (a, a, a) -> (a, a, a) -> a
diff (x1, y1, z1) (x2, y2, z2) = abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)

bestNextPerm :: (Ord a, Num a) => (a, a, a) -> (a, a, a) -> (a, a, a)
bestNextPerm prev curr =
  snd $ minimum $ (diff prev &&& id) <$> allPerms curr

takovai :: [(a, a, a)] -> ([a], [a], [a])
takovai ts = (fst3 <$> ts, snd3 <$> ts, trd3 <$> ts)
  where
    fst3 (a, _, _) = a
    snd3 (_, b, _) = b
    trd3 (_, _, c) = c

ottakovai :: ([a], [a], [a]) -> [(a, a, a)]
ottakovai = uncurry3 zip3
  where
    uncurry3 f (a, b, c) = f a b c

---

combStreams :: (Ord a, Num a) => ([a], [a], [a]) -> ([a], [a], [a])
combStreams = takovai . helper . ottakovai
  where
    helper og@((x,y,z):rest) = (x,y,z) : zipWith bestNextPerm (helper og) rest
    helper [] = error "Finite lists not supported"
