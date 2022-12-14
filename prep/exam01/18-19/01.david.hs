-- https://learn.fmi.uni-sofia.bg/mod/page/view.php?id=110489

-- >>> middleDigit 452
-- 5

-- >>> middleDigit 4712
-- -1

natToDigits :: Integer -> [Integer]
natToDigits n 
  | n < 0 = natToDigits (-n)
  | n == 0 = []
  | otherwise = n `rem` 10 : natToDigits (n `quot` 10)

middleDigit :: Integer -> Integer
middleDigit n = case natToDigits n of
  [] -> 0
  digits -> if even len then -1 else digits !! (len `rem` 2)
    where
      len = length digits
