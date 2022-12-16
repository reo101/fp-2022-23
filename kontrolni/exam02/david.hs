------------
-- TASK 1 --
------------ 

--------
-- a) --
-------- 

type Team = String
type Result = (Team, Team, Integer, Integer)
type Tournament = [Result]

resultInvolves :: Result -> Team -> Bool
resultInvolves (t1, t2, _, _) t = t `elem` [t1, t2]

wins :: Result -> Team -> Bool
wins r@(t1, t2, g1, g2) t
  | not $ resultInvolves r t = False
  | otherwise = g1 > g2 && t == t1  || g2 > g1 && t == t2

getTeams :: Result -> (Team, Team)
getTeams (t1, t2, _, _) = (t1, t2)

getPoints :: Team -> Result -> Integer
getPoints t r 
  | wins r t = 3
  | otherwise = 0

getAllTeams :: Tournament -> [Team]
getAllTeams = foldr accumulator []
  where 
    accumulator result acc = addIfNot (snd teams) $ addIfNot (fst teams) acc
      where 
        teams = getTeams result
        addIfNot x xs = if x `elem` xs then xs else x : xs

filterTeam :: Tournament -> Team -> Tournament
filterTeam tour t = filter involves tour
  where
    involves result = t `elem` getTeamsList result
    getTeamsList (t1, t2, _, _) = [t1, t2]

getAllPoints :: Tournament -> Team -> Integer
getAllPoints tour t =  sum $ map (getPoints t) $ filterTeam tour t

getGoalDifference :: Tournament -> Team -> Integer
getGoalDifference tour t = sum $ map (getDiff t) $ filterTeam tour t
  where
    getDiff team result = if team == fst (getTeams result) then fst diff else snd diff
      where
        diff = getDiff result
          where
            getDiff (_, _, g1, g2) = (g1 - g2, g2 - g1)

getMaxPoints :: Tournament -> Integer
getMaxPoints tour = maximum $ map (getAllPoints tour) $ getAllTeams tour

getTeamsWithMaxPoints :: Tournament -> [Team]
getTeamsWithMaxPoints tour = filter (\team -> getAllPoints tour team == maxPts) $ getAllTeams tour
  where
    maxPts = getMaxPoints tour

maxPointsMinGoal :: Tournament -> Team
maxPointsMinGoal tour = snd $ minimum $ map (\team -> (getGoalDifference tour team, team)) $ getTeamsWithMaxPoints tour

tournament = [("A", "B",1,0), ("B", "C", 4,1), ("B", "C", 0,4), ("B", "A",1,2), ("A","C",0,1)]

-- >>> maxPointsMinGoal tournament
-- "A"

--------
-- b) --
-------- 

exceedSelf :: Tournament -> [Team]
exceedSelf tour = filter (\t -> exceeds t t) $ getAllTeams tour
  where

    exceeds :: Team -> Team -> Bool
    exceeds a b = 
         (a /= b && any (\r -> resultInvolves r b && wins r a) tour) -- either a != b and a has a direct win over b
      || any -- or there exists
          (\c -> --a team c
               c /= b --which is not b (b might be == a)
            && exceeds c b --and exceeds b
            && all (`wins` a) (getDirectMatches c a) --and `a` has defeated `c` in all their direct matches
          ) (getDirectlyDefeatedTeams a) --amongst all the teams that a has defeated
    
    --some utility functions below
    getDirectMatches t1 t2 = filter (\r -> resultInvolves r t1 && resultInvolves r t2) tour

    getDirectlyDefeatedTeams a = map getOpponent $ filter (`wins` a) tour
      where
        getOpponent (t1, t2, _, _)
          | a == t1 = t2
          | otherwise = t1

-- >>> exceedSelf tournament
-- ["C"]

------------
-- TASK 2 --
------------ 

data Next = End | Branch Node Node -- a follow up of a node is either a an end (the node is a leaf), or a branching
data Node = Node IntPred Next -- a node contains a predicate and a follow up

-- -- alternatively
-- data Node = Leaf IntPred | Branch IntPred Node Node

type IntPred = (Integer -> Bool) -- predicate type (shorthand)
type Trace = [Bool] --the sequence of paths

getTrace :: Node -> Integer -> Trace
getTrace (Node f End) x = [f x]
getTrace (Node f (Branch ifFalse ifTrue)) x = 
  f x : getTrace (if f x then ifTrue else ifFalse) x

sameTrace :: Node -> [Integer] -> Bool
sameTrace t [] = False
sameTrace t xs = any (\x -> any (\y -> x /= y && getTrace t x == getTrace t y) xs) xs

-- tests
sampleTree = Node (< 2) (Branch 
  (Node odd (Branch 
    (Node (> 6) End) 
    (Node (`elem` [2, 3]) End))) --a mock of isPrime for the example
  (Node (> 0) End))

-- >>> sameTrace sampleTree [1,3,4] 
-- False
-- >>> sameTrace sampleTree [1,3,4,9,2] 
-- True

------------
-- TASK 3 --
------------ 


combStreams :: [Integer] -> [Integer] -> [Integer] -> [[Integer]]
combStreams (a : as) (b : bs) (c : cs) = accumulator as bs cs [a, b, c]
  where
    accumulator (a : as) (b : bs) (c : cs) prev = 
      prev : accumulator as bs cs (minPermutation a b c prev)

    minPermutation a b c prev = 
      snd 
      $ minimum 
      $ map (\perm -> (getComparableValue prev perm, perm)) 
      $ getPermutations a b c  
      
    getComparableValue next prev = 
      sum $ zipWith (\n p -> abs $ n - p) next prev
    
    getPermutations a b c = [[a,b,c], [a,c,b], [b,a,c], [b,c,a], [c,a,b], [c,b,a]]
