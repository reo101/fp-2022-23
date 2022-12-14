listsEqual :: Eq a => [a] -> [a] -> Bool
listsEqual l1 l2 = all (`elem` l2) l1 && all (`elem` l1) l2

-- >>> isEndomorphism [0, 1, 4, 6] (+) (`rem` 3)
-- True

isEndomorphism :: Eq a => [a] -> (a -> a -> a) -> (a -> a) -> Bool
isEndomorphism xs op f =
  let 
    image = map f xs
    allResults list = map (\x -> map (op x) list) list
    opOriginal = allResults xs
    opImage = allResults image
  in listsEqual opImage (map (map f) opOriginal)
