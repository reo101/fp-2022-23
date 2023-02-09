{-# LANGUAGE NamedFieldPuns #-}

-------------
-- helpers --
-------------

find :: (a -> Bool) -> [a] -> Maybe a
find p [] = Nothing
find p (x : xs) = if p x then Just x else find p xs

-----------
---- 1 ----
-----------
somos :: Int -> [Int]
somos k = ones ++ go ones
  where
    ones = replicate k 1

    -- based on our last k-1-length window, we decide on the next element
    -- in which we have first * next = sum of products of simetrichno vlojeni dvoiki within (first, last] (i.e. (first, next))
    go :: [Int] -> [Int]
    go (x : xs) = next : go (xs ++ [next])
      where
        next = 
            sum --sum
              (take (k `div` 2) -- all of the
                $ zipWith (*) xs (reverse xs)) -- products of inner symmetric pairs (the inners are exactly k div 2 na broi)
          `quot` --and divide that
            x --by the first element of the window (equivalent to the uslovieto)
    -- again, ugly, but go will generate an endless list which will have a constant size list
    -- validatin the size of a [] was not that trivial for a kontrolno time, taka che ostanahme bez fancy "parse, don't validate" istorii :D
    go _ = error "no way"

------------------------------------
---- No time left for the bonus ----
------------------------------------
-- I'm not even quite sure I get it - is this a computational problem or a mathematical one with a constructive computational proof?
-- Like, ain't that a constant one-time solution independent of any input? * thinking emoji *

-----------
---- 2 ----
-----------
type State = Int
data NKA = NKA
  { states :: [State]
  , initial :: State
  , edges :: [(State, Char, State)]
  , final :: [State]
  }

a, b :: Char
a = 'a'
b = 'b'

alphabet :: String
alphabet = [a,b]

-- generate a list of all words with length <= n
allWords :: Int -> [String]
allWords 0 = [""]
allWords n = prev ++ concatMap (\c -> (c :) <$> wordsPreviousLength) alphabet
  where
    prev = allWords (n - 1)
    wordsPreviousLength = filter ((== n - 1) . length) prev

-- check whether the automaton accepts the given word
accepts :: NKA -> String -> Bool
accepts NKA {states, initial, edges, final} = go initial
  where
    go state "" = state `elem` final
    go state (c : cs) = 
      any 
        (\(sFrom, ch, sTo) -> 
          sFrom == state && 
          ch == c && 
          go sTo cs) 
        edges

exampleNKA :: NKA
exampleNKA = NKA
  { states = [0, 1, 2]
  , initial = 0
  , edges = [(0, a, 1), (1, b, 1), (1, a, 2), (2, a, 2), (2, b, 1)]
  , final = [0, 1, 2]
  }

-- Based on the hint, we want an exhaustive list of all possible inputs
-- to brute force this problem exponentially :D
-- just checking if there is a single word amongst all the words from our alphabet
-- with length <= 2^n that is not accepted
-- otherwise, nothing

-- >>> rejectedWord exampleNKA
-- Just "b"
-- >>> rejectedWord exampleNKA{edges = (0, b, 1) : edges exampleNKA}
-- Nothing
rejectedWord :: NKA -> Maybe String
rejectedWord nka@(NKA {states, initial, edges, final}) =
  find (not . accepts nka) $ allWords $ 2 ^ length states

-----------
---- 3 ----
-----------
-- >>> findMin [(+), (-), (*)] [1,2,0,3,-1]
-- -7
findMin :: (Num a, Ord a) => [a -> a -> a] -> [a] -> a
-- some corner cases... perhaps errors would be better?
findMin fs [] = 0
findMin fs [x] = 0
--the actual solution
findMin fs xs = minimum allCycleEvals
  where
    -- create a list of length m with all possible cyclic rotations of the m provided functions
    allFunctionCycles = map (\i -> drop i $ cycle fs) [0 .. length fs - 1]
    -- for each rotation, evaluate with the provided x values
    allCycleEvals = evaluate xs <$> allFunctionCycles

    evaluate [x1, x2] (f : _) = f x1 x2
    evaluate (x : xs) (f : fs) = f x $ evaluate xs fs

    -- ugly, but for completeness...
    -- I didn't have time to think of a better one... validated at least in the parent function
    evaluate _ _ = error "impossible" 
