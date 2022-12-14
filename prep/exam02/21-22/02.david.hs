--------
-- a) --
--------

avg :: [Double] -> Int -> Double
avg s n = 
    sum (take n s) / fromIntegral n

-- >>> take 4 $ movingAverage [1076,1356,1918,6252,6766,5525, 0] 3
-- [1450.0,3175.3333333333335,4978.666666666667,6181.0]

movingAverage :: [Double] -> Int -> [Double]
movingAverage s n =
    avg s n : movingAverage (tail s) n

--------
-- b) --
--------

-- >>> take 3 $ map (take 4) $ allAverages [1076, 1356, 1918, 6252, 6766, 5525, 0 ]
-- [[1216.0,1637.0,4085.0,6509.0],[1450.0,3175.3333333333335,4978.666666666667,6181.0],[2650.5,4073.0,5115.25,4635.75]]

allAverages :: [Double] -> [[Double]]
allAverages s = map (movingAverage s) [2..]
