Module with a key value BST.

> module KVBST (
>   KVBST,
>   setValueInKVBST, -- :: Ord a => (a, b) -> KVBST a b -> KVBST a b
>   getValueInKVBST, -- :: Ord a => a -> KVBST a b -> b
>   makeKVBST        -- :: Ord a => [(a,b)] -> KVBST a b
> )
> where

> import Data.List
> import Data.Function

> data KVBST a b = Empty | Fork (KVBST a b) (a, b) (KVBST a b)

> setValueInKVBST :: Ord a => a -> b -> KVBST a b -> KVBST a b
> setValueInKVBST k v Empty                     = Fork Empty (k,v) Empty
> setValueInKVBST k v (Fork l (y,z) r) | k < y  = Fork (setValueInKVBST k v l) (y,z) r
>                                      | k > y  = Fork l (y,z) (setValueInKVBST k v r)
>                                      | k == y = Fork l (y,v) r

> getValueInKVBST :: Ord a => a -> KVBST a b -> b
> getValueInKVBST k Empty                     = error "No element with such key in the treeQ"
> getValueInKVBST k (Fork l (y,z) r) | k < y  = getValueInKVBST k l
>                                    | k > y  = getValueInKVBST k r
>                                    | k == y = z

> makeKVBST :: Ord a => [(a,b)] -> KVBST a b
> makeKVBST [] = Empty
> makeKVBST xs = Fork (makeKVBST lh) m (makeKVBST rh)
>     where srt  = sortBy (compare `on` fst) xs
>           half = length srt `div` 2
>           (lh,m:rh) = splitAt half srt
