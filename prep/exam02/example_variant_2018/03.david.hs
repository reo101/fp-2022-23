-- >>> meetTwice id negate (-3) 1
-- False

-- >>> meetTwice id sqrt 0 5
-- True

meetTwice :: (Num b, Enum b, Eq a) => (b -> a) -> (b -> a) -> b -> b -> Bool
meetTwice f g a b = 
  let 
    range = [a .. b]
    fRange = map f range
    gRange = map g range
    bothResults = zipWith (==) fRange gRange
  in
    length (filter id bothResults) >= 2
