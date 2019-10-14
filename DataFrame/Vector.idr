module Vector

import Data.List
import Data.Vect

%default total

public export
data Vector : Nat -> Type -> Type where
  CVector : (n : Nat) -> List x -> Vector n x

export
empty : Vector Z x
empty = CVector Z []

export
singleton : x -> Vector (S Z) x
singleton x = CVector (S Z) [x]

export
cons : x -> Vector n x -> Vector (S n) x
cons x (CVector n xs) = CVector (S n) (x :: xs)

export
fromList : List x -> (k : Nat ** (Vector k x))
fromList xs = let le = length xs in (le ** CVector le xs)

export
app : Vector n x -> Vector m x -> Vector (n+m) x
app (CVector n xs) (CVector m ys) = CVector (n+m) (xs ++ ys)

export
fromVect : Vect k x -> Vector k x
fromVect [] = CVector Z []
fromVect (y :: xs) = (singleton y) `app` fromVect xs

export
toList : Vector n x -> List x
toList (CVector _ xs) = xs

partial
lindex : Nat -> List x -> x
lindex Z (x :: xs) = x
lindex (S n) (x :: xs) = lindex n xs

export
partial
index : {n : Nat} -> (f : Fin n) -> (xs : Vector n x) -> x
index f (CVector n xs) = lindex (finToNat f) xs

export
implementation Show x => Show (Vector len x) where
  show = show .toList
