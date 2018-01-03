average :: Float -> Float -> Float
average x y = (x + y) / 2

improve :: Float -> Float -> Float
improve guess x = average guess (x / guess)

square :: Float -> Float
square x = x * x

goodEnough :: Float -> Float -> Bool
goodEnough guess x =
        let precision = 0.001
        in  abs (square guess - x) < precision

sqrtIter :: Float -> Float -> Float
sqrtIter guess x
        | goodEnough guess x = guess
        | otherwise          = sqrtIter (improve guess x) x

sqrt :: Float -> Float
sqrt x = sqrtIter 1.0 x
