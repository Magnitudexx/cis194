toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0            = []
    | n < 10            = [n]
    | otherwise         = toDigits (n `div`10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0            = []
    | n < 10            = [n]
    | otherwise         = n `mod` 10 : toDigitsRev (n `div`10)

getMultiplyer :: [Integer] -> Integer
getMultiplyer (x:xs) = (fromIntegral(length (x:xs) +1) `mod` 2) +1
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []     = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:xs) = getMultiplyer (x:xs) * x : doubleEveryOther xs
