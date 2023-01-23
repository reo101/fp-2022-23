type Graph a = Eq a => [(a, [a])]

getParents :: Eq a => Graph a -> a -> [a]
getParents g u = 
  map fst $ filter (\(_, children) -> u `elem` children) g

getChildren :: Eq a => Graph a -> a -> [a]
getChildren g u = 
  snd $ head $ filter (\(node, _) -> u == node) g

isFamily :: Eq a => [a] -> Graph a -> Bool
isFamily nodes graph = 
  all nodeInFamily nodes
  where
    nodeInFamily node =
         firstAllSecondNone children parents 
      || firstAllSecondNone parents children
      where
        parents = getParents graph node
        children = getChildren graph node
        firstAllSecondNone first second = 
             all (`elem` nodes) first 
          && all (`notElem` nodes) second

minIncluding :: Eq a => a -> Graph a -> [a]
minIncluding node graph
  | isFamily startingWithParents graph = startingWithParents
  | isFamily startingWithChildren graph = startingWithChildren
  | otherwise = error "No such family"
  where
    startingWithParents = relatives True [node]
    startingWithChildren = relatives False [node]

    nextGetter ofParents = (if ofParents then getParents else getChildren) graph

    relatives ofParents acc
      | result == acc = result
      | otherwise     = relatives (not ofParents) result
      where
        result = foldr (\x a -> filter (`notElem` a) (nextGetter ofParents x) ++ a) acc acc
