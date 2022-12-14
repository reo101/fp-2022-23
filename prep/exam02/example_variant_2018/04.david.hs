type AccType = (Maybe Integer, Integer, [Integer])

-- >>> nextLookAndSay [1, 1, 2, 3, 3]
-- [2,1,1,2,2,3]

nextLookAndSay :: [Integer] -> [Integer]
nextLookAndSay = 
  addLastToAccumulator .
  foldr accumulator (Nothing, 0, [])
  where
    accumulator :: Integer -> AccType -> AccType
    accumulator x (Nothing, _, acc) = (Just x, 1, acc)
    accumulator x (Just n, count, acc)
      | n == x = (Just x, 1 + count, acc)
      | otherwise = (Just x, 1, count : n : acc)
      
    addLastToAccumulator :: AccType -> [Integer]
    addLastToAccumulator (Nothing, _, xs) = xs
    addLastToAccumulator (Just x, count, xs) = count : x : xs
