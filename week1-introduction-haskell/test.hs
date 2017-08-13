-- test.hs

double x = x + x
quadruple x = double (double x)
factorial n = product [1..n]
avg ns = div (sum ns) (length ns)
productC [] = 1
productC (n:ns) = n * productC ns
insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x ys
sumtorial :: Int -> Int
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)
