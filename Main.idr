module Main

import Data.Vect
import Data.HVect

%default total

provedElemIndex : {xs : Vect k a} -> Elem x xs -> Fin k
provedElemIndex Here = FZ
provedElemIndex (There p) = FS (provedElemIndex p)

-- RList is short for Named heterogeneous vector
data RList : Vect k Type -> Vect k String -> Type where
  Nil : RList [] []
  (::) : t ->
         {n : String} ->
         RList ts ns ->
         RList (t :: ts) (n :: ns)

names : {ns : Vect k String} -> RList ts ns -> Vect k String
names Nil = []
names ((::) _ {n} rs) = (n :: names rs)

index : (i : Fin k) -> RList ts ns -> index i ts
index FZ (v ::  _) = v
index (FS f) (_ :: vs) = index f vs

labelType : (n : String) -> (ns : Vect k String) ->
            (ts : Vect k Type) -> Elem n ns -> Type

labelType _ _ ts p = index (provedElemIndex p) ts

label : (n : String) -> RList ts ns ->
        {auto p : Elem n ns} -> labelType n ns ts p

label _ ( v :: _ ) {p = Here} = v
label n ( _ :: vs) {p = There p'} = label n vs {p = p'}

updateAt : (i : Fin n) -> (index i ts -> t) -> RList ts ns ->
           RList (replaceAt i t ts) ns

updateAt FZ f (v :: vs) = f v :: vs
updateAt (FS j) f (v :: vs) = v :: updateAt j f vs

updateAtLabel : {ns : Vect k String} ->
                (n : String) -> RList ts ns ->
                {auto p : Elem n ns} ->
                (labelType n ns ts p -> t) ->
                RList (replaceAt (provedElemIndex p) t ts) ns

--updateAtLabel _ {p} f vs = Main.updateAt (provedElemIndex p) f vs
updateAtLabel {ns = n :: _} _ (v :: vs) {p = Here} f = f v :: vs
updateAtLabel {ns = _ :: ns} n (v :: vs) {p = There p} f  =
  v :: updateAtLabel {ns = ns} n vs {p = p} f

-- List Examples

myList : RList [String, Integer] ["Name", "Age"]
myList = ["John", 29]

-- Data Frame

-- DF : Nat -> Vect k Type -> Vect k String -> Type
-- DF nRows types names = RList (Vect nRows <$> types) names
--
-- addRow : {ts : Vect (S k) Type} ->
--          HVect ts ->
--          DF nRows ts ns ->
--          DF (S nRows) ts ns
