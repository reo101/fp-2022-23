import Data.List ( (\\) )

makeRange :: Integer -> [Integer]
makeRange n = [0 .. n - 1]

--------
-- a) --
--------

-- >>> isNPerm 3 (\x -> (3 - x) `mod` 3)
-- True
-- >>> isNPerm 10 (`div` 2)
-- False
-- >>> isNPerm 10 (\x -> (x + 2) `mod` 10)
-- True

isNPerm :: Integer -> (Integer -> Integer) -> Bool
isNPerm n f =
  all (`elem` mapped) range && all (`elem` range) mapped
  where
    mapped :: [Integer]
    mapped = map f range

    range :: [Integer]
    range = makeRange n

--------
-- b) --
--------

-- >>> maxCycle 3 (\x -> (3 - x) `mod` 3)
-- [1,2]
-- >>> maxCycle 10 (\x -> (x + 2) `mod` 10)
-- [0,2,4,6,8]
-- >>> maxCycle 10 (\x -> (x + 3) `mod` 10)
-- [0,3,6,9,2,5,8,1,4,7]

getLongerFoldable :: Foldable t => t a -> t a -> t a
getLongerFoldable l1 l2 = if length l1 >= length l2 then l1 else l2

maxCycle :: Integer -> (Integer -> Integer) -> [Integer]
maxCycle n f = maxLen (makeRange n)
  where
    maxLen [] = []
    maxLen (x : xs) = 
      getLongerFoldable starting $ maxLen $ xs \\ starting
      where
        starting = x : takeWhile (/= x) (tail $ iterate f x)
