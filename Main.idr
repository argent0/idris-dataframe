module Main

import RList
import Data.Vect

%default total

sampleDF : RList [Vect 3 String, Vect 3 Double] ["Name", "Age"]
sampleDF = [["John", "Mike", "Trevor"], [25, 30, 35]]

meanFolder : Num a => (a, a) -> a -> (a, a)
meanFolder (psum, plen) e = (psum + e, plen + 1)

mean : Fractional a => Vect (S k) a -> a
mean vs = uncurry (/) $ foldl meanFolder (0, 0) vs 
