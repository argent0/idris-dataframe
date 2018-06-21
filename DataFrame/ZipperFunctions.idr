module ZipperFunctions

import Data.Vect

%access public export
%default total

data ZipperFunctions : Vect k Type -> Vect k Type -> Vect k Type -> Type where
  Nil : ZipperFunctions [] [] []
  (::) : 
    (f : a -> b -> c) ->
    ZipperFunctions as bs cs -> ZipperFunctions (a :: as) (b :: bs) (c :: cs)

replicate : (k : Nat) -> (f : a -> b -> c) ->
            ZipperFunctions (replicate k a) (replicate k b) (replicate k c) 

replicate Z _ = Nil
replicate (S k) f = f :: replicate k f
