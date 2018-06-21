module DataFrame

import DataFrame.RList
import DataFrame.ZipperFunctions

import Data.Vect

%access public export
%default total

data DataFrame : Nat -> Vect n Type -> Vect n String -> Type where
  Nil : DataFrame nRow [] [] --A DataFrame with no columns but maybe rows
  (::) : Vect nRow t ->
       {n : String} ->
        DataFrame nRow ts ns -> DataFrame nRow (t :: ts) (n :: ns)
   
-- A DataFrame with no rows but maybe columns
empty : {ts : Vect k Type} ->
        {ns : Vect k String} -> DataFrame Z ts ns
empty {ts= Nil} {ns= Nil} = Nil
empty {ts= (t :: ts) } {ns = (n :: ns)} = Nil :: empty {ts} {ns}

-- Concatenate rows
rcons : {ts : Vect k Type} ->
        {nRow : Nat} ->
        RList ts ns -> 
        DataFrame nRow ts ns ->
        DataFrame (S nRow) ts ns

rcons {nRow} Nil Nil = the (DataFrame (S nRow) [] []) Nil
rcons (a :: as) (v :: vs) = (a :: v) :: rcons as vs

col : DataFrame nRow ts ns -> (i : Fin k) -> Vect nRow (index i ts)
col (v ::  _) FZ  = v
col (_ :: vs) (FS f) = col vs f

label : DataFrame nRow ts ns -> (n : String) ->
        {auto p : Elem n ns} -> Vect nRow (labelType n ns ts p)

label ( v :: _ ) _ {p = Here} = v
label ( _ :: vs) n {p = There p'} = label vs n {p = p'}

row : DataFrame nRow ts ns -> Fin nRow -> RList ts ns
row Nil _ = Nil
row (v :: vs) k = index k v :: row vs k

sampleDF : DataFrame 3 [String, Double] ["Name", "Age"]
sampleDF = [["John", "Mike", "Trevor"], [25, 30, 35]]

sampleDF' : DataFrame 3 [String, Double] ["Name", "Age"]
sampleDF' = rcons ["John", 25] $
            rcons ["Mike", 30] $
            rcons ["Trevor", 35] empty

-- Instances

Show (DataFrame k [] []) where
   show _ = "Nil"

(Show (DataFrame k ts ns), Show t) => Show (DataFrame k (t :: ts) (nm :: ns)) where
   show (c :: cs) = (show c) ++ " :: " ++ (show cs)

-- Convenience functions

||| If the given dataframe has the desired number of rows, return a dataframe
||| with that number of rows, otherwise, return Nothing
||| @nrows the required number of rows
||| @df the tested data frame
exactRowNumber : {k : Nat} -> -- expected at runtim
   (nrows : Nat) -> 
   (df : DataFrame k ts ns) -> Maybe (DataFrame nrows ts ns)

exactRowNumber {k} nrows ds with (decEq k nrows)
   exactRowNumber {k = k} k ds | (Yes Refl) = Just ds
   exactRowNumber {k = k} nrows ds | (No contra) = Nothing

-- vim: expandtab
