{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMultiSet
-- Copyright   :  (c) Twan van Laarhoven 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An efficient implementation of multisets of integers, also somtimes called bags.
--
-- A multiset is like a set, but it can contain multiple copies of the same element.
--
-- Since many function names (but not the type name) clash with
-- "Prelude" names, this module is usually imported @qualified@, e.g.
--
-- >  import Data.MultiSet (MultiSet)
-- >  import qualified Data.MultiSet as MultiSet
--
-- The implementation of 'MultiSet' is based on the "Data.IntMap" module.
--
-- Many operations have a worst-case complexity of /O(min(n,W))/.
-- This means that the operation can become linear in the number of
-- elements with a maximum of /W/ -- the number of bits in an 'Int'
-- (32 or 64). Here /n/ refers to the number of distinct elements,
-- /t/ is the total number of elements.
-----------------------------------------------------------------------------

module Data.IntMultiSet  ( 
            -- * MultiSet type
              IntMultiSet, Key, Occur

            -- * Operators
            , (\\)

            -- * Query
            , null
            , size
            , distinctSize
            , member
            , notMember
            , occur
            , isSubsetOf
            , isProperSubsetOf

            -- * Construction
            , empty
            , singleton
            , insert
            , insertMany
            , delete
            , deleteMany
            , deleteAll

            -- * Combine
            , union, unions
            , maxUnion
            , difference
            , intersection

            -- * Filter
            , filter
            , partition
            , split
            , splitOccur

            -- * Map
            , map
            , mapMonotonic
            , mapMaybe
            , mapEither
            , concatMap
            , unionsMap

            -- * Monadic
            , bind
            , join

            -- * Fold
            , fold
            , foldOccur

            -- * Min\/Max
            , findMin
            , findMax
            , deleteMin
            , deleteMax
            , deleteMinAll
            , deleteMaxAll
            , deleteFindMin
            , deleteFindMax
            , maxView
            , minView

            -- * Conversion

            -- ** List
            , elems
            , distinctElems
            , toList
            , fromList

            -- ** Ordered list
            , toAscList
            , fromAscList
            , fromDistinctAscList

            -- ** Occurrence lists
            , toOccurList
            , toAscOccurList
            , fromOccurList
            , fromAscOccurList
            , fromDistinctAscOccurList

            -- ** Map
            , toMap
            , fromMap
            , fromOccurMap

            -- ** Set
            , toSet
            , fromSet

            -- * Debugging
            , showTree
            , showTreeWith
            ) where

import Prelude hiding (filter,foldr,null,map,concatMap
#if __GLASGOW_HASKELL__ >= 709 && __GLASGOW_HASKELL__ < 710
  ,join
#endif
  )
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (Monoid(..))
#endif
import Data.Typeable ()
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.MultiSet (MultiSet)
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set
import qualified Data.List as List
import qualified Data.MultiSet as MultiSet

{-
-- just for testing
import QuickCheck 
import List (nub,sort)
import qualified List
-}

import Data.Typeable
#if __GLASGOW_HASKELL__
import Text.Read
import Data.Data (Data(..), mkNoRepType)
#endif

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 \\ --

-- | /O(n+m)/. See 'difference'.
(\\) :: IntMultiSet -> IntMultiSet -> IntMultiSet
m1 \\ m2 = difference m1 m2

{--------------------------------------------------------------------
  The data type
--------------------------------------------------------------------}

-- | A multiset of integers.
--   The same value can occur multiple times.
newtype IntMultiSet = MS { unMS :: IntMap Occur }
                     -- invariant: all values in the map are >= 1

type Key = Int

-- | The number of occurences of an element
type Occur = Int

instance Monoid IntMultiSet where
    mempty  = empty
    mappend = union
    mconcat = unions

#if __GLASGOW_HASKELL__

{--------------------------------------------------------------------
  A Data instance  
--------------------------------------------------------------------}

-- This instance preserves data abstraction at the cost of inefficiency.
-- We omit reflection services for the sake of data abstraction.

instance Data IntMultiSet where
  gfoldl f z set = z fromList `f` (toList set)
  toConstr _     = error "toConstr"
  gunfold _ _    = error "gunfold"
  dataTypeOf _   = mkNoRepType "Data.IntMultiSet.IntMultiSet"

#endif

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | /O(1)/. Is this the empty multiset?
null :: IntMultiSet -> Bool
null = Map.null . unMS

-- | /O(n)/. The number of elements in the multiset.
size :: IntMultiSet -> Int
size = sum . Map.elems . unMS

-- | /O(1)/. The number of distinct elements in the multiset.
distinctSize :: IntMultiSet -> Int
distinctSize = Map.size . unMS

-- | /O(min(n,W))/. Is the element in the multiset?
member :: Key -> IntMultiSet -> Bool
member x = Map.member x . unMS

-- | /O(min(n,W))/. Is the element not in the multiset?
notMember :: Key -> IntMultiSet -> Bool
notMember x = not . member x

-- | /O(min(n,W))/. The number of occurences of an element in a multiset.
occur :: Key -> IntMultiSet -> Int
occur x = Map.findWithDefault 0 x . unMS

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}

-- | /O(1)/. The empty mutli set.
empty :: IntMultiSet
empty = MS Map.empty

-- | /O(1)/. Create a singleton mutli set.
singleton :: Key -> IntMultiSet
singleton x = MS (Map.singleton x 1)

{--------------------------------------------------------------------
  Insertion, Deletion
--------------------------------------------------------------------}

-- | /O(min(n,W))/. Insert an element in a multiset.
insert :: Key -> IntMultiSet -> IntMultiSet
insert x = MS . Map.insertWith (+) x 1 . unMS

-- | /O(min(n,W))/. Insert an element in a multiset a given number of times.
--
-- Negative numbers remove occurences of the given element.
insertMany :: Key -> Occur -> IntMultiSet -> IntMultiSet
insertMany x n
 | n <  0    = MS . Map.update (deleteN (negate n)) x . unMS
 | n == 0    = id
 | otherwise = MS . Map.insertWith (+) x n . unMS

-- | /O(min(n,W))/. Delete a single element from a multiset.
delete :: Key -> IntMultiSet -> IntMultiSet
delete x = MS . Map.update (deleteN 1) x . unMS

-- | /O(min(n,W))/. Delete an element from a multiset a given number of times.
--
-- Negative numbers add occurences of the given element.
deleteMany :: Key -> Occur -> IntMultiSet -> IntMultiSet
deleteMany x n = insertMany x (negate n)

-- | /O(min(n,W))/. Delete all occurences of an element from a multiset.
deleteAll :: Key -> IntMultiSet -> IntMultiSet
deleteAll x = MS . Map.delete x . unMS

deleteN :: Int -> Int -> Maybe Int
deleteN n m
  | m <= n    = Nothing
  | otherwise = Just (m - n)


{--------------------------------------------------------------------
  Subset
--------------------------------------------------------------------}

-- | /O(n+m)/. Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: IntMultiSet -> IntMultiSet -> Bool
isProperSubsetOf (MS m1) (MS m2) = Map.isProperSubmapOfBy (<=) m1 m2

-- | /O(n+m)/. Is this a subset?
-- @(s1 \`isSubsetOf\` s2)@ tells whether @s1@ is a subset of @s2@.
isSubsetOf :: IntMultiSet -> IntMultiSet -> Bool
isSubsetOf (MS m1) (MS m2) = Map.isSubmapOfBy (<=) m1 m2

{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}

-- | /O(log n)/. The minimal element of a multiset.
findMin :: IntMultiSet -> Key
-- TODO: IntMap has a different findMin than Map
--findMin = fst . Map.findMin . unMS
--findMin = Map.findMin . unMS

-- | /O(log n)/. The maximal element of a multiset.
findMax :: IntMultiSet -> Key
-- TODO: IntMap has a different findMin than Map
--findMax = fst . Map.findMax . unMS
--findMax = Map.findMax . unMS

-- Note: the documentation for IntMap.findMin/Max is incorrect
-- they return the VALUE at the minimal/maximal key

-- Here is a workarounds of IntMap's deficiencies/inconsistencies.

-- | /O(log n)/. The minimal key of an IntMap.
minKey :: IntMap a -> Int
minKey = maybe (error "IntMultiSet.findMin: empty multiset") (fst . fst) . Map.minViewWithKey

-- | /O(log n)/. The maximal key of an IntMap.
maxKey :: IntMap a -> Int
maxKey = maybe (error "IntMultiSet.findMax: empty multiset") (fst . fst) . Map.maxViewWithKey

findMin = minKey . unMS
findMax = maxKey . unMS


-- | /O(log n)/. Delete the minimal element.
deleteMin :: IntMultiSet -> IntMultiSet
deleteMin = MS . Map.updateMin (deleteN 1) . unMS

-- | /O(log n)/. Delete the maximal element.
deleteMax :: IntMultiSet -> IntMultiSet
deleteMax = MS . Map.updateMax (deleteN 1) . unMS

-- | /O(log n)/. Delete all occurences of the minimal element.
deleteMinAll :: IntMultiSet -> IntMultiSet
deleteMinAll m = MS . Map.deleteMin . unMS $ m

-- | /O(log n)/. Delete all occurences of the maximal element.
deleteMaxAll :: IntMultiSet -> IntMultiSet
deleteMaxAll m = MS . Map.deleteMax . unMS $ m

-- | /O(log n)/. Delete and find the minimal element.
-- 
-- > deleteFindMin set = (findMin set, deleteMin set)
deleteFindMin :: IntMultiSet -> (Key, IntMultiSet)
-- TODO: get updateFindMin added to Data.IntMap
--deleteFindMin = (\((v,_),m) -> (v, MS m)) . Map.updateFindMin (deleteN 1) . unMS
deleteFindMin set = (findMin set, deleteMin set)


-- | /O(log n)/. Delete and find the maximal element.
-- 
-- > deleteFindMax set = (findMax set, deleteMax set)
deleteFindMax :: IntMultiSet -> (Key,IntMultiSet)
-- TODO: get updateFindMax added to Data.IntMap
--deleteFindMax = (\((v,_),m) -> (v, MS m)) . Map.updateFindMax (deleteN 1) . unMS
deleteFindMax set = (findMax set, deleteMax set)

-- | /O(log n)/. Retrieves the minimal element of the multiset, and the set stripped from that element
-- Returns @Nothing@ when passed an empty multiset.
--
-- Examples:
-- >>> minView $ fromList [100, 100, 200, 300]
-- Just (100,fromOccurList [(100,1),(200,1),(300,1)])
minView :: IntMultiSet -> Maybe (Key, IntMultiSet)
minView x
  | null x    = Nothing
  | otherwise = Just (deleteFindMin x)

-- | /O(log n)/. Retrieves the maximal element of the multiset, and the set stripped from that element
-- @fail@s (in the monad) when passed an empty multiset.
--
-- Examples:
-- >>> maxView $ fromList [100, 100, 200, 300]
-- Just (300,fromOccurList [(100,2),(200,1)])
maxView :: IntMultiSet -> Maybe (Key, IntMultiSet)
maxView x
  | null x    = Nothing
  | otherwise = Just (deleteFindMax x)

{--------------------------------------------------------------------
  Union, Difference, Intersection
--------------------------------------------------------------------}

-- | The union of a list of multisets: (@'unions' == 'foldl' 'union' 'empty'@).
unions :: [IntMultiSet] -> IntMultiSet
unions ts
  = foldlStrict union empty ts

-- | /O(n+m)/. The union of two multisets. The union adds the occurences together.
--
-- The implementation uses the efficient /hedge-union/ algorithm.
-- Hedge-union is more efficient on (bigset `union` smallset).
union :: IntMultiSet -> IntMultiSet -> IntMultiSet
union (MS m1) (MS m2) = MS $ Map.unionWith (+) m1 m2

-- | /O(n+m)/. The union of two multisets.
-- The number of occurences of each element in the union is
-- the maximum of the number of occurences in the arguments (instead of the sum).
--
-- The implementation uses the efficient /hedge-union/ algorithm.
-- Hedge-union is more efficient on (bigset `union` smallset).
maxUnion :: IntMultiSet -> IntMultiSet -> IntMultiSet
maxUnion (MS m1) (MS m2) = MS $ Map.unionWith max m1 m2

-- | /O(n+m)/. Difference of two multisets. 
-- The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
difference :: IntMultiSet -> IntMultiSet -> IntMultiSet
difference (MS m1) (MS m2) = MS $ Map.differenceWith (flip deleteN) m1 m2

-- | /O(n+m)/. The intersection of two multisets.
--
-- prints @(fromList [A],fromList [B])@.
intersection :: IntMultiSet -> IntMultiSet -> IntMultiSet
intersection (MS m1) (MS m2) = MS $ Map.intersectionWith min m1 m2

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}
-- | /O(n)/. Filter all elements that satisfy the predicate.
filter :: (Key -> Bool) -> IntMultiSet -> IntMultiSet
filter p = MS . Map.filterWithKey (\k _ -> p k) . unMS

-- | /O(n)/. Partition the multiset into two multisets, one with all elements that satisfy
-- the predicate and one with all elements that don't satisfy the predicate.
-- See also 'split'.
partition :: (Key -> Bool) -> IntMultiSet -> (IntMultiSet,IntMultiSet)
partition p = (\(x,y) -> (MS x, MS y)) . Map.partitionWithKey (\k _ -> p k) . unMS

{----------------------------------------------------------------------
  Map
----------------------------------------------------------------------}

-- | /O(n*log n)/. 
-- @'map' f s@ is the multiset obtained by applying @f@ to each element of @s@.
map :: (Key->Key) -> IntMultiSet -> IntMultiSet
-- TODO: IntMap doesn't have a mapKeys function
map f = fromOccurList . List.map (\(x,o) -> (f x, o)) . toOccurList

-- | /O(n)/. The 
--
-- @'mapMonotonic' f s == 'map' f s@, but works only when @f@ is strictly monotonic.
-- /The precondition is not checked./
-- Semi-formally, we have:
-- 
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapMonotonic f s == map f s
-- >     where ls = toList s
mapMonotonic :: (Key->Key) -> IntMultiSet -> IntMultiSet
mapMonotonic f = fromAscOccurList . List.map (\(x,o) -> (f x, o)) . toAscOccurList

-- | /O(n)/. Map and collect the 'Just' results.
mapMaybe :: (Key -> Maybe Key) -> IntMultiSet -> IntMultiSet
mapMaybe f = fromOccurList . mapMaybe' . toOccurList
  where mapMaybe' [] = []
        mapMaybe' ((x,n):xs) = case f x of
           Just x' -> (x',n) : mapMaybe' xs
           Nothing ->          mapMaybe' xs

-- | /O(n)/. Map and separate the 'Left' and 'Right' results.
mapEither :: (Key -> Either Key Key) -> IntMultiSet -> (IntMultiSet, IntMultiSet)
mapEither f = (\(ls,rs) -> (fromOccurList ls, fromOccurList rs)) . mapEither' . toOccurList
  where mapEither' [] = ([],[])
        mapEither' ((x,n):xs) = case f x of
           Left  l -> let (ls,rs) = mapEither' xs in ((l,n):ls, rs)
           Right r -> let (ls,rs) = mapEither' xs in (ls, (r,n):rs)


-- | /O(n)/. Apply a function to each element, and take the union of the results
concatMap :: (Key -> [Key]) -> IntMultiSet -> IntMultiSet
concatMap f = fromOccurList . Map.foldrWithKey mapF [] . unMS
  where mapF x occ rest = List.map (\y -> (y,occ)) (f x) ++ rest

-- | /O(n)/. Apply a function to each element, and take the union of the results
unionsMap :: (Key -> IntMultiSet) -> IntMultiSet -> IntMultiSet
unionsMap f = unions . List.map timesF . toOccurList
  where timesF (ms,1) = f ms
        timesF (ms,n) = MS . Map.map (*n) . unMS $ f ms

-- | /O(n)/. The monad join operation for multisets.
join :: MultiSet IntMultiSet -> IntMultiSet
join = unions . List.map times . MultiSet.toOccurList
  where times (ms,1) = ms
        times (ms,n) = MS . Map.map (*n) . unMS $ ms

-- | /O(n)/. The monad bind operation, (>>=), for multisets.
bind :: IntMultiSet -> (Key -> IntMultiSet) -> IntMultiSet
bind = flip unionsMap

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}

-- | /O(t)/. Fold over the elements of a multiset in an unspecified order.
fold :: (Key -> b -> b) -> b -> IntMultiSet -> b
fold f z s
  = foldr f z s

-- | /O(t)/. Post-order fold.
foldr :: (Key -> b -> b) -> b -> IntMultiSet -> b
foldr f z = Map.foldrWithKey repF z . unMS
 where repF a 1 b = f a b
       repF a n b = repF a (n - 1) (f a b)

-- | /O(n)/. Fold over the elements of a multiset with their occurences.
foldOccur :: (Key -> Occur -> b -> b) -> b -> IntMultiSet -> b
foldOccur f z = Map.foldrWithKey f z . unMS

{--------------------------------------------------------------------
  List variations 
--------------------------------------------------------------------}
-- | /O(t)/. The elements of a multiset.
elems :: IntMultiSet -> [Key]
elems = toList

-- | /O(n)/. The distinct elements of a multiset, each element occurs only once in the list.
--
-- > distinctElems = map fst . toOccurList
distinctElems :: IntMultiSet -> [Key]
distinctElems = Map.keys . unMS

{--------------------------------------------------------------------
  Lists 
--------------------------------------------------------------------}
-- | /O(t)/. Convert the multiset to a list of elements.
toList :: IntMultiSet -> [Key]
toList = toAscList

-- | /O(t)/. Convert the multiset to an ascending list of elements.
toAscList :: IntMultiSet -> [Key]
toAscList = foldr (:) []

-- | /O(t*min(n,W))/. Create a multiset from a list of elements.
fromList :: [Int] -> IntMultiSet 
fromList xs = fromOccurList $ zip xs (repeat 1)

-- | /O(t)/. Build a multiset from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: [Int] -> IntMultiSet 
fromAscList xs = fromAscOccurList $ zip xs (repeat 1)

-- | /O(n)/. Build a multiset from an ascending list of distinct elements in linear time.
-- /The precondition (input list is strictly ascending) is not checked./
fromDistinctAscList :: [Int] -> IntMultiSet 
fromDistinctAscList xs = fromDistinctAscOccurList $ zip xs (repeat 1)

{--------------------------------------------------------------------
  Occurence lists 
--------------------------------------------------------------------}

-- | /O(n)/. Convert the multiset to a list of element\/occurence pairs.
toOccurList :: IntMultiSet -> [(Int,Int)]
toOccurList = toAscOccurList

-- | /O(n)/. Convert the multiset to an ascending list of element\/occurence pairs.
toAscOccurList :: IntMultiSet -> [(Int,Int)]
toAscOccurList = Map.toAscList . unMS


-- | /O(n*min(n,W))/. Create a multiset from a list of element\/occurence pairs.
fromOccurList :: [(Int,Int)] -> IntMultiSet 
fromOccurList = MS . Map.fromListWith (+)

-- | /O(n)/. Build a multiset from an ascending list of element\/occurence pairs in linear time.
-- /The precondition (input list is ascending) is not checked./
fromAscOccurList :: [(Int,Int)] -> IntMultiSet 
fromAscOccurList = MS . Map.fromAscListWith (+)

-- | /O(n)/. Build a multiset from an ascending list of elements\/occurence pairs where each elements appears only once.
-- /The precondition (input list is strictly ascending) is not checked./
fromDistinctAscOccurList :: [(Int,Int)] -> IntMultiSet 
fromDistinctAscOccurList = MS . Map.fromDistinctAscList

{--------------------------------------------------------------------
  Map
--------------------------------------------------------------------}

-- | /O(1)/. Convert a multiset to an 'IntMap' from elements to number of occurrences.
toMap :: IntMultiSet -> IntMap Int
toMap = unMS

-- | /O(n)/. Convert an 'IntMap' from elements to occurrences to a multiset.
fromMap :: IntMap Int -> IntMultiSet
fromMap = MS . Map.filter (>0)

-- | /O(1)/. Convert an 'IntMap' from elements to occurrences to a multiset.
-- Assumes that the 'IntMap' contains only values larger than one.
-- /The precondition (all elements > 1) is not checked./
fromOccurMap :: IntMap Int -> IntMultiSet
fromOccurMap = MS

{--------------------------------------------------------------------
  Set
--------------------------------------------------------------------}

-- | /O(n)/. Convert a multiset to an 'IntMap', removing duplicates.
toSet :: IntMultiSet -> IntSet
toSet = Map.keysSet . unMS

-- | /O(n)/. Convert an 'IntMap' to a multiset.
fromSet :: IntSet -> IntMultiSet
fromSet = fromDistinctAscList . Set.toAscList

{--------------------------------------------------------------------
  Instances 
--------------------------------------------------------------------}

instance Eq IntMultiSet where
  m1 == m2  =  unMS m1 == unMS m2

instance Ord IntMultiSet where
  compare s1 s2 = compare (unMS s1) (unMS s2)
  {-
  -- compare s1 s2 = compare (toAscList s1) (toAscList s2) 
  -- We want {x,x,y} < {x,y}
  -- i.e. if the number of occurences differ, more occurences come first.
  -- But also, {x,x} > {x}
  -- so this does not hold at the end of the list.
  --
  -- To summarize:
  --    * [(x,2),(y,1)] < [(x,1),(y,1)]
  --    * [(x,2)      ] < [(x,1),(y,1)]
  --    * [(x,2),(y,1)] > [(x,1)      ]
  --    * [(x,2)      ] > [(x,1)      ]
  compare s1 s2 = comp (toAscOccurList s1) (toAscOccurList s2) 
    where comp []         []     = EQ
          comp []         (_:_)  = LT
          comp (_:_)      []     = GT
          comp ((x,n):xs) ((y,m):ys)
             = case compare x y of
                 EQ    -> case compare n m of
                            EQ -> comp xs ys
                            LT -> case xs of
                                    [] -> LT
                                    _  -> GT
                            GT -> case ys of
                                    [] -> GT
                                    _  -> LT
                 other -> other
  -}

instance Show IntMultiSet where
  showsPrec p xs = showParen (p > 10) $
    showString "fromOccurList " . shows (toOccurList xs)

{--------------------------------------------------------------------
  Read
--------------------------------------------------------------------}
instance Read IntMultiSet where
#ifdef __GLASGOW_HASKELL__
  readPrec = parens $ prec 10 $ do
    Ident "fromOccurList" <- lexP
    xs <- readPrec
    return (fromOccurList xs)

  readListPrec = readListPrecDefault
#else
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("fromOccurList",s) <- lex r
    (xs,t) <- reads s
    return (fromOccurList xs,t)
#endif

{--------------------------------------------------------------------
  Typeable/Data
--------------------------------------------------------------------}

#include "Typeable.h"
INSTANCE_TYPEABLE0(IntMultiSet,intMultiSetTc,"IntMultiSet")

{--------------------------------------------------------------------
  Split
--------------------------------------------------------------------}

-- | /O(log n)/. The expression (@'split' x set@) is a pair @(set1,set2)@
-- where all elements in @set1@ are lower than @x@ and all elements in
-- @set2@ larger than @x@. @x@ is not found in neither @set1@ nor @set2@.
split :: Int -> IntMultiSet -> (IntMultiSet,IntMultiSet)
split a = (\(x,y) -> (MS x, MS y)) . Map.split a . unMS

-- | /O(log n)/. Performs a 'split' but also returns the number of
-- occurences of the pivot element in the original set.
splitOccur :: Int -> IntMultiSet -> (IntMultiSet,Int,IntMultiSet)
splitOccur a (MS t) = let (l,m,r) = Map.splitLookup a t in
     (MS l, maybe 0 id m, MS r)

{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}

-- TODO : Use foldl' from base?
foldlStrict :: (a -> t -> a) -> a -> [t] -> a
foldlStrict f z xs
  = case xs of
      []     -> z
      (x:xx) -> let z' = f z x in seq z' (foldlStrict f z' xx)


{--------------------------------------------------------------------
  Debugging
--------------------------------------------------------------------}
-- | /O(n)/. Show the tree that implements the set. The tree is shown
-- in a compressed, hanging format.
showTree :: IntMultiSet -> String
showTree s = showTreeWith True False s


{- | /O(n)/. The expression (@showTreeWith hang wide map@) shows
 the tree that implements the set. If @hang@ is
 @True@, a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.

> Set> putStrLn $ showTreeWith True False $ fromDistinctAscList [1,1,2,3,4,5]
> (1*) 4
> +--(1*) 2
> |  +--(2*) 1
> |  +--(1*) 3
> +--(1*) 5
> 
> Set> putStrLn $ showTreeWith True True $ fromDistinctAscList [1,1,2,3,4,5]
> (1*) 4
> |
> +--(1*) 2
> |  |
> |  +--(2*) 1
> |  |
> |  +--(1*) 3
> |
> +--(1*) 5
> 
> Set> putStrLn $ showTreeWith False True $ fromDistinctAscList [1,1,2,3,4,5]
> +--(1*) 5
> |
> (1*) 4
> |
> |  +--(1*) 3
> |  |
> +--(1*) 2
>    |
>    +--(2*) 1

-}
showTreeWith :: Bool -> Bool -> IntMultiSet -> String
showTreeWith hang wide = Map.showTreeWith hang wide . unMS
