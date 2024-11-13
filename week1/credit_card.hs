--having n < 10 is redundant because it is handled by recursion
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0            = []
    | otherwise         = toDigits (n `div`10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0            = []
    | otherwise         = n `mod` 10 : toDigitsRev (n `div`10)
{-
necessary code see below
getMultiplyer :: [Integer] -> Integer
getMultiplyer (x:xs)    = (fromIntegral(length (x:xs) +1) `mod` 2) +1
-}
doubleEveryOther :: [Integer] -> [Integer]
{-
this can be done simpler
doubleEveryOther (x:xs) = getMultiplyer (x:xs) * x : doubleEveryOther xs

zipWith zips two lists and than runs function on pair
cycle links last element of list to first element of list
-}
doubleEveryOther []     = []
doubleEveryOther [x]    = [x]
doubleEveryOther xs     = reverse (zipWith (*) (cycle [1, 2]) (reverse xs))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = sum (toDigits x)
--sumDigits (x:xs) = sum (map (sum . toDigits) (x:xs)) simpler method below
sumDigits xs    = sum(concatMap toDigits xs) --(concatMap concatenates resulting list of map)

validate :: Integer -> Bool
validate n = n > 0 && sumDigits (doubleEveryOther(toDigits n)) `mod` 10 == 0
