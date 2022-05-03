-- main = do
--   putStrLn "The average of four numbers is: "
--   print(average 1.0 2.0 3.0 4.0)
-- -- Question 1a
-- average :: Float -> Float -> Float -> Float -> Float
-- average a b c d = foldl1 (+) [a,b,c,d] / 4

-- cd OneDrive\Desktop\projects\a4_301.hs
-- :set -XOverloadedStrings
-- :set -XRecordWildCards
-- :l weatherstats-template.hs
-- :l weatherstats.hs
-- jan3Minimum "2069-2021.csv"

-- average :: (Fractional a, Foldable t) => t a -> a
average :: [Float] -> Float
average xs = foldl1 (+) xs / fromIntegral (length xs)

-- maxDiff :: (Num a, Foldable t, Ord a) => t a -> a
maxDiff :: [Float] -> Float
maxDiff xs = maximum xs - minimum xs

