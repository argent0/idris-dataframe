module DataFrame

import RList
import ZipperFunctions

import Data.Vect

%access public export
%default total

data DataFrame : Nat -> Vect n Type -> Vect n String -> Type where
  Nil : DataFrame nRow [] []
  (::) : Vect nRow t ->
       {n : String} ->
        DataFrame nRow ts ns -> DataFrame nRow (t :: ts) (n :: ns)
   
-- A DataFrame with no rows but maube columns
empty : {ts : Vect k Type} ->
        {ns : Vect k String} -> DataFrame Z ts ns
empty {ts= Nil} {ns= Nil} = Nil
empty {ts= (t :: ts) } {ns = (n :: ns)} = Nil :: empty {ts} {ns}

rcons : {ts : Vect k Type} ->
        {nRow : Nat} ->
        RList ts ns -> 
        DataFrame nRow ts ns ->
        DataFrame (S nRow) ts ns

rcons {nRow} Nil Nil = the (DataFrame (S nRow) [] []) Nil
rcons (a :: as) (v :: vs) = (a :: v) :: rcons as vs

sampleDF : DataFrame 3 [String, Double] ["Name", "Age"]
sampleDF = [["John", "Mike", "Trevor"], [25, 30, 35]]

sampleDF' : DataFrame 3 [String, Double] ["Name", "Age"]
sampleDF' = rcons ["John", 25] $
            rcons ["Mike", 30] $
            rcons ["Trevor", 35] empty

-- vim: expandtab
