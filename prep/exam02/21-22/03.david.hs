import Data.Bifunctor (second)

type Inventory = [(String, [String])]

sampleInventory :: Inventory
sampleInventory = [ ("docs", ["ids", "invoices"]), ("ids", ["passport"]),  ("invoices", []), ("memes", []),
        ("family", ["new year", "birthday"]), ("funny", ["memes"]), ("pics", ["family", "funny"]) ]

--------
-- a) --
--------

getBoxLabels :: Inventory -> [String]
getBoxLabels = map fst

-- >>> allObjects sampleInventory
-- ["passport","new year","birthday"]

allObjects :: Inventory -> [String]
allObjects inv = 
  filter (`notElem` getBoxLabels inv) $ concatMap snd inv

--------
-- b) --
--------

-- >>> cleanUpEmpty sampleInventory
-- [("docs",["ids"]),("ids",["passport"]),("family",["new year","birthday"]),("pics",["family"])]

cleanUpEmpty :: Inventory -> Inventory
cleanUpEmpty inv
  | null $ getEmptyBoxLabels result = result
  | otherwise                       = cleanUpEmpty result
    where
      getEmptyBoxLabels = getBoxLabels . filter (null . snd)
      notAnEmptyBoxLabel = (`notElem` getEmptyBoxLabels inv)
      notAnEmptyBox = notAnEmptyBoxLabel . fst
      result = map (second (filter notAnEmptyBoxLabel)) $ filter notAnEmptyBox inv

-- >>> cleanUp sampleInventory
-- [("docs",["passport"]),("pics",["new year","birthday"])]

cleanUpSingles :: Inventory -> Inventory
cleanUpSingles inv
  | null labelsOfBoxesToRemove = inv
  | otherwise                  = cleanUpSingles result
  where
    isBoxLabel :: String -> Bool
    isBoxLabel = (`elem` getBoxLabels inv)

    containsOnlyOneBox :: [String] -> Bool
    containsOnlyOneBox [item] = isBoxLabel item
    containsOnlyOneBox _ = False

    labelsOfBoxesToRemove :: [String]
    labelsOfBoxesToRemove = getBoxLabels $ filter (containsOnlyOneBox . snd) inv

    getBoxContent :: String -> [String]
    getBoxContent label = snd $ head $ filter ((== label) . fst) inv

    result :: Inventory
    result = filter ((`elem` labelsOfBoxesToRemove) . fst) 
      $ map (second replaceLabelWithBoxContents) inv
      where
        replaceLabelWithBoxContents items
          | containsOnlyOneBox items = getBoxContent $ head items
          | otherwise                = items

cleanUp :: Inventory -> Inventory
cleanUp = cleanUpSingles . cleanUpEmpty