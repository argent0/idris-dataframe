module Main

import Data.Vect

%default total

||| A proof that some element is found in a vector
data VElem : a -> Vect k a -> Type where
  ||| A proof that the element is at the front of the list
  Here : VElem x (x :: xs)
  ||| A proof that the element is after the front of the list
  There : VElem x xs -> VElem x (y :: xs)

-- RList is short for Named heterogeneous vector
data RList : Vect k Type -> Vect k String -> Type where
  Nil : RList [] []
  Cons : t -> String -> RList ts ns -> RList (t :: ts) (n :: ns)

names : {ns : Vect k String} -> RList ts ns -> Vect k String
names Nil = []
names (Cons _ n rs) = (n :: names rs)

index : (i : Fin k) -> RList ts ns -> index i ts
index FZ (Cons v _ _) = v
index (FS f) (Cons _ _ vs) = index f vs

labelType : (n : String) ->
            (ns : Vect k String) ->
            (ts : Vect k Type) ->
            VElem n ns -> Type
labelType n _ (t :: _) Here = t
labelType n (_ :: ns) (_ :: ts) (There p) = labelType n ns ts p

label : (n : String) ->
        RList ts ns ->
        {auto p : VElem n ns} ->
        labelType n ns ts p

label _ (Cons v _ _) {p = Here} = v
label n (Cons _ _ vs) {p = There p'} = label n vs {p = p'}

-- Examples

myList : RList [String, Integer] ["Name", "Age"]
myList = Cons "John" "Name" (Cons 29 "Age" Nil)
