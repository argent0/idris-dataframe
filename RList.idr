module RList

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

label : RList ts ns -> (n : String) ->
        {auto p : Elem n ns} -> labelType n ns ts p

label ( v :: _ ) _ {p = Here} = v
label ( _ :: vs) n {p = There p'} = label vs n {p = p'}

updateAt : RList ts ns -> (i : Fin n) -> (index i ts -> t) -> 
           RList (replaceAt i t ts) ns

updateAt (v :: vs) FZ f  = f v :: vs
updateAt (v :: vs) (FS j) f  = v :: updateAt vs j f

updateAtLabel : 
                {ns : Vect k String} ->
                RList ts ns -> (n : String) ->
                {auto p : Elem n ns} ->
                {default n nn : String} ->
                (labelType n ns ts p -> t) ->
                RList (replaceAt (provedElemIndex p) t ts)
                      (replaceAt (provedElemIndex p) nn ns)

updateAtLabel {ns = n :: _} (v :: vs) n {p = Here} {nn = nn} f = f v :: vs
updateAtLabel {ns = _ :: ns} (v :: vs) n {p = There p} {nn = nn} f  =
  v :: updateAtLabel {ns = ns} vs n {p = p} {nn = nn} f

preppendLabel : {ns : Vect k String} ->
              {nt : Type} ->
              RList ts ns -> (n : String) ->
              (v : nt) -> RList (nt :: ts) (n :: ns)

preppendLabel vs n v = RList.(::) v vs

-- List Examples

sampleList : RList [String, Integer] ["Name", "Age"]
sampleList = ["John", 29]
