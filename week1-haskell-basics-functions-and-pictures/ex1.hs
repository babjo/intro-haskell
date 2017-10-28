toDigits :: Integer -> [Integer]
toDigits n
    | n < 10 = [n]
    | otherwise =  toDigits (div n 10) ++ [mod n 10]
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
	| n < 10 = [n]
    | otherwise =  mod n 10 : toDigitsRev (div n 10)