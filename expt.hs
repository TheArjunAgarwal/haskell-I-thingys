fight :: Int -> Int -> Double
fight 1 b = 720 / fromIntegral b
fight a b = if 3 * a > b then (-1) else fig a b

fig :: Int -> Int -> Double
fig a b
  | x < 1     = (-1)  -- Return 0 if x is less than 1 to avoid an empty list
  | otherwise = maximum [720 / fromIntegral (3 + m) + fightm (a - x - 1 + m) b | m <- [1..x]]
  where
    x = b - 3 * a

fightm :: Int -> Int -> Double
fightm a b
  | a < 1     = 0  -- Return 0 if index is out of bounds
  | otherwise = (map (\x -> fight x b) [1..]) !! (a-1)
