improve :: Float -> Float -> Float
improve guess x = (x / guess^2 + 2*guess) / 3 

goodEnough :: Float -> Float -> Bool
goodEnough guess x =
        let precision = 0.001
        in  abs (guess^3 - x) < precision

cubeRootIter :: Float -> Float -> Float
cubeRootIter guess x
        | goodEnough guess x = guess
        | otherwise          = cubeRootIter (improve guess x) x

cubeRoot :: Float -> Float
cubeRoot x = cubeRootIter 1.0 x
