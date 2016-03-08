module Main

import Data.Vect

%default total

-- RList is short for Named heterogeneous vector
data RList : Vect k Type -> Vect k String -> Type where
  Nil : RList [] []
  Cons : t -> String -> RList ts ns -> RList (t :: ts) (n :: ns)

names : {ns : Vect k String} -> RList ts ns -> Vect k String
names Nil = []
names (Cons _ n rs) = (n :: names rs)

-- Examples

myList : RList [String, Integer] ["Name", "Age"]
myList = Cons "John" "Name" (Cons 29 "Age" Nil)
