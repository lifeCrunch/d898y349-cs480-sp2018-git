--Tyler Miller
--D898Y349
dos:: Int->Int
dos x 
    |x <= 0 = 0
    |otherwise = (sqSum(x))^2 - sumSq(x)

sumSq:: Int->Int
sumSq x
    |x <= 0 = 0
    |x > 0 = x^2 + sumSq(x-1)
    |otherwise = 0

sqSum::Int->Int
sqSum x
    |x <= 0 = 0
    |x > 0 = x + sqSum(x-1)
    |otherwise = 0

s2w::String->[String]
s2w x
    |length x <= 0 = []
    |[head x] == " " = s2w(tail x)
    |otherwise = [split(x)]++s2w(clip(x))++[]

split::String->String
split (x:xs)
    |length xs <= 0 = [x]
    |[x] == " " = []
    |otherwise = [x]++split(xs)++[]

clip::String->String
clip (x:xs)
    |length xs <= 0 = []
    |[x] == " " = xs
    |otherwise = clip(xs)