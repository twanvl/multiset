{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.MultiSet
-- Copyright   :  (c) Twan van Laarhoven 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An efficient implementation of multisets, also somtimes called bags.
--
-- A multiset is like a set, but it can contain multiple copies of the same element.
-- Unless otherwise specified all insert and remove opertions affect only a single copy of an element.
-- For example the minimal element before and after @deleteMin@ could be the same, only with one less occurence.
--
-- Since many function names (but not the type name) clash with
-- "Prelude" names, this module is usually imported @qualified@, e.g.
--
-- >  import Data.MultiSet (MultiSet)
-- >  import qualified Data.MultiSet as MultiSet
--
-- The implementation of 'MultiSet' is based on the "Data.Map" module.
--
-- Note that the implementation is /left-biased/ -- the elements of a
-- first argument are always preferred to the second, for example in
-- 'union' or 'insert'.  Of course, left-biasing can only be observed
-- when equality is an equivalence relation instead of structural
-- equality.
--
-- In the complexity of functions /n/ refers to the number of distinct elements,
-- /t/ is the total number of elements.
-----------------------------------------------------------------------------

module Data.MultiSet  ( 
            -- * MultiSet type
              MultiSet, Occur

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
            , valid
            ) where

import Prelude hiding (filter,foldr,null,map,concatMap)
import Data.Monoid (Monoid(..))
import Data.Typeable ()
import qualified Data.Foldable as Foldable (Foldable(foldr))
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List

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
(\\) :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
m1 \\ m2 = difference m1 m2

{--------------------------------------------------------------------
  The data type
--------------------------------------------------------------------}

-- | A multiset of values @a@.
--   The same value can occur multiple times.
newtype MultiSet a = MS { unMS :: Map a Occur }
                     -- invariant: all values in the map are >= 1

-- | The number of occurences of an element
type Occur = Int

instance Ord a => Monoid (MultiSet a) where
    mempty  = empty
    mappend = union
    mconcat = unions

instance Foldable.Foldable MultiSet where
    foldr = fold

#if __GLASGOW_HASKELL__

{--------------------------------------------------------------------
  A Data instance  
--------------------------------------------------------------------}

-- This instance preserves data abstraction at the cost of inefficiency.
-- We omit reflection services for the sake of data abstraction.

instance (Data a, Ord a) => Data (MultiSet a) where
  gfoldl f z set = z fromList `f` (toList set)
  toConstr _     = error "toConstr"
  gunfold _ _    = error "gunfold"
  dataTypeOf _   = mkNoRepType "Data.MultiSet.MultiSet"
  dataCast1 f    = gcast1 f

#endif

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | /O(1)/. Is this the empty multiset?
null :: MultiSet a -> Bool
null = Map.null . unMS

-- | /O(n)/. The number of elements in the multiset.
size :: MultiSet a -> Occur
size = sum . Map.elems . unMS

-- | /O(1)/. The number of distinct elements in the multiset.
distinctSize :: MultiSet a -> Occur
distinctSize = Map.size . unMS

-- | /O(log n)/. Is the element in the multiset?
member :: Ord a => a -> MultiSet a -> Bool
member x = Map.member x . unMS

-- | /O(log n)/. Is the element not in the multiset?
notMember :: Ord a => a -> MultiSet a -> Bool
notMember x = not . member x

-- | /O(log n)/. The number of occurences of an element in a multiset.
occur :: Ord a => a -> MultiSet a -> Occur
occur x = Map.findWithDefault 0 x . unMS

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}

-- | /O(1)/. The empty mutli set.
empty :: MultiSet a
empty = MS Map.empty

-- | /O(1)/. Create a singleton mutli set.
singleton :: a -> MultiSet a
singleton x = MS (Map.singleton x 1)

{--------------------------------------------------------------------
  Insertion, Deletion
--------------------------------------------------------------------}

-- | /O(log n)/. Insert an element in a multiset.
insert :: Ord a => a -> MultiSet a -> MultiSet a
insert x = MS . Map.insertWith (+) x 1 . unMS

-- | /O(log n)/. Insert an element in a multiset a given number of times.
--
-- Negative numbers remove occurences of the given element.
insertMany :: Ord a => a -> Occur -> MultiSet a -> MultiSet a
insertMany x n
 | n <  0    = MS . Map.update (deleteN (negate n)) x . unMS
 | n == 0    = id
 | otherwise = MS . Map.insertWith (+) x n . unMS

-- | /O(log n)/. Delete a single element from a multiset.
delete :: Ord a => a -> MultiSet a -> MultiSet a
delete x = MS . Map.update (deleteN 1) x . unMS

-- | /O(log n)/. Delete an element from a multiset a given number of times.
--
-- Negative numbers add occurences of the given element.
deleteMany :: Ord a => a -> Occur -> MultiSet a -> MultiSet a
deleteMany x n = insertMany x (negate n)

-- | /O(log n)/. Delete all occurences of an element from a multiset.
deleteAll :: Ord a => a -> MultiSet a -> MultiSet a
deleteAll x = MS . Map.delete x . unMS

deleteN :: Occur -> Occur -> Maybe Occur
deleteN n m
  | m <= n    = Nothing
  | otherwise = Just (m - n)


{--------------------------------------------------------------------
  Subset
--------------------------------------------------------------------}

-- | /O(n+m)/. Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: Ord a => MultiSet a -> MultiSet a -> Bool
isProperSubsetOf (MS m1) (MS m2) = Map.isProperSubmapOfBy (<=) m1 m2

-- | /O(n+m)/. Is this a subset?
-- @(s1 \`isSubsetOf\` s2)@ tells whether @s1@ is a subset of @s2@.
isSubsetOf :: Ord a => MultiSet a -> MultiSet a -> Bool
isSubsetOf (MS m1) (MS m2) = Map.isSubmapOfBy (<=) m1 m2

{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}

-- | /O(log n)/. The minimal element of a multiset.
findMin :: MultiSet a -> a
findMin = fst . Map.findMin . unMS

-- | /O(log n)/. The maximal element of a multiset.
findMax :: MultiSet a -> a
findMax = fst . Map.findMax . unMS

-- | /O(log n)/. Delete the minimal element.
deleteMin :: MultiSet a -> MultiSet a
deleteMin = MS . Map.updateMin (deleteN 1) . unMS

-- | /O(log n)/. Delete the maximal element.
deleteMax :: MultiSet a -> MultiSet a
deleteMax = MS . Map.updateMax (deleteN 1) . unMS

-- | /O(log n)/. Delete all occurences of the minimal element.
deleteMinAll :: MultiSet a -> MultiSet a
deleteMinAll = MS . Map.deleteMin . unMS

-- | /O(log n)/. Delete all occurences of the maximal element.
deleteMaxAll :: MultiSet a -> MultiSet a
deleteMaxAll = MS . Map.deleteMax . unMS

-- | /O(log n)/. Delete and find the minimal element.
-- 
-- > deleteFindMin set = (findMin set, deleteMin set)
deleteFindMin :: MultiSet a -> (a, MultiSet a)
-- TODO: add this missing function to Data.Map
--deleteFindMin = (\((v,_),m) -> (v, MS m)) . Map.updateFindMin (deleteN 1) . unMS
deleteFindMin set = (findMin set, deleteMin set)

-- | /O(log n)/. Delete and find the maximal element.
-- 
-- > deleteFindMax set = (findMax set, deleteMax set)
deleteFindMax :: MultiSet a -> (a,MultiSet a)
-- TODO: add this missing function to Data.Map
--deleteFindMax = (\((v,_),m) -> (v, MS m)) . Map.updateFindMax (deleteN 1) . unMS
deleteFindMax set = (findMax set, deleteMax set)

-- | /O(log n)/. Retrieves the minimal element of the multiset,
--   and the set with that element removed.
--   @fail@s (in the monad) when passed an empty multiset.
minView :: Monad m => MultiSet a -> m (a, MultiSet a)
minView x
  | null x    = fail "MultiSet.minView: empty multiset"
  | otherwise = return (deleteFindMin x)

-- | /O(log n)/. Retrieves the maximal element of the multiset,
--   and the set with that element removed.
--   @fail@s (in the monad) when passed an empty multiset.
maxView :: Monad m => MultiSet a -> m (a, MultiSet a)
maxView x
  | null x    = fail "MultiSet.maxView: empty multiset"
  | otherwise = return (deleteFindMin x)

{--------------------------------------------------------------------
  Union, Difference, Intersection
--------------------------------------------------------------------}

-- | The union of a list of multisets: (@'unions' == 'foldl' 'union' 'empty'@).
unions :: Ord a => [MultiSet a] -> MultiSet a
unions ts
  = foldlStrict union empty ts

-- | /O(n+m)/. The union of two multisets. The union adds the occurences together.
-- 
-- The implementation uses the efficient /hedge-union/ algorithm.
-- Hedge-union is more efficient on (bigset `union` smallset).
union :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
union (MS m1) (MS m2) = MS $ Map.unionWith (+) m1 m2

-- | /O(n+m)/. The union of two multisets.
-- The number of occurences of each element in the union is
-- the maximum of the number of occurences in the arguments (instead of the sum).
--
-- The implementation uses the efficient /hedge-union/ algorithm.
-- Hedge-union is more efficient on (bigset `union` smallset).
maxUnion :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
maxUnion (MS m1) (MS m2) = MS $ Map.unionWith max m1 m2

-- | /O(n+m)/. Difference of two multisets. 
-- The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
difference :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
difference (MS m1) (MS m2) = MS $ Map.differenceWith (flip deleteN) m1 m2

-- | /O(n+m)/. The intersection of two multisets.
-- Elements of the result come from the first multiset, so for example
--
-- > import qualified Data.MultiSet as MS
-- > data AB = A | B deriving Show
-- > instance Ord AB where compare _ _ = EQ
-- > instance Eq AB where _ == _ = True
-- > main = print (MS.singleton A `MS.intersection` MS.singleton B,
-- >               MS.singleton B `MS.intersection` MS.singleton A)
--
-- prints @(fromList [A],fromList [B])@.
intersection :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
intersection (MS m1) (MS m2) = MS $ Map.intersectionWith min m1 m2

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}
-- | /O(n)/. Filter all elements that satisfy the predicate.
filter :: Ord a => (a -> Bool) -> MultiSet a -> MultiSet a
filter p = MS . Map.filterWithKey (\k _ -> p k) . unMS

-- | /O(n)/. Partition the multiset into two multisets, one with all elements that satisfy
-- the predicate and one with all elements that don't satisfy the predicate.
-- See also 'split'.
partition :: Ord a => (a -> Bool) -> MultiSet a -> (MultiSet a,MultiSet a)
partition p = (\(x,y) -> (MS x, MS y)) . Map.partitionWithKey (\k _ -> p k) . unMS

{----------------------------------------------------------------------
  Map
----------------------------------------------------------------------}

-- | /O(n*log n)/. 
-- @'map' f s@ is the multiset obtained by applying @f@ to each element of @s@.
map :: (Ord b) => (a->b) -> MultiSet a -> MultiSet b
map f = MS . Map.mapKeysWith (+) f . unMS

-- | /O(n)/. The 
--
-- @'mapMonotonic' f s == 'map' f s@, but works only when @f@ is strictly monotonic.
-- /The precondition is not checked./
-- Semi-formally, we have:
-- 
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapMonotonic f s == map f s
-- >     where ls = toList s
mapMonotonic :: (a->b) -> MultiSet a -> MultiSet b
mapMonotonic f = MS . Map.mapKeysMonotonic f . unMS

-- | /O(n)/. Map and collect the 'Just' results.
mapMaybe :: (Ord b) => (a -> Maybe b) -> MultiSet a -> MultiSet b
mapMaybe f = fromOccurList . mapMaybe' . toOccurList
  where mapMaybe' [] = []
        mapMaybe' ((x,n):xs) = case f x of
           Just x' -> (x',n) : mapMaybe' xs
           Nothing ->          mapMaybe' xs

-- | /O(n)/. Map and separate the 'Left' and 'Right' results.
mapEither :: (Ord b, Ord c) => (a -> Either b c) -> MultiSet a -> (MultiSet b, MultiSet c)
mapEither f = (\(ls,rs) -> (fromOccurList ls, fromOccurList rs)) . mapEither' . toOccurList
  where mapEither' [] = ([],[])
        mapEither' ((x,n):xs) = case f x of
           Left  l -> let (ls,rs) = mapEither' xs in ((l,n):ls, rs)
           Right r -> let (ls,rs) = mapEither' xs in (ls, (r,n):rs)


-- | /O(n)/. Apply a function to each element, and take the union of the results
concatMap :: (Ord b) => (a -> [b]) -> MultiSet a -> MultiSet b
concatMap f = fromOccurList . Map.foldrWithKey mapF [] . unMS
  where mapF x occ rest = List.map (\y -> (y,occ)) (f x) ++ rest

-- | /O(n)/. Apply a function to each element, and take the union of the results
unionsMap :: (Ord b) => (a -> MultiSet b) -> MultiSet a -> MultiSet b
unionsMap f = unions . List.map timesF . toOccurList
  where timesF (ms,1) = f ms
        timesF (ms,n) = MS . Map.map (*n) . unMS $ f ms

-- | /O(n)/. The monad join operation for multisets.
join :: Ord a => MultiSet (MultiSet a) -> MultiSet a
join = unions . List.map times . toOccurList
  where times (ms,1) = ms
        times (ms,n) = MS . Map.map (*n) . unMS $ ms

-- | /O(n)/. The monad bind operation, (>>=), for multisets.
bind :: (Ord b) => MultiSet a -> (a -> MultiSet b) -> MultiSet b
bind = flip unionsMap

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}

-- | /O(t)/. Fold over the elements of a multiset in an unspecified order.
fold :: (a -> b -> b) -> b -> MultiSet a -> b
fold f z s
  = foldr f z s

-- | /O(t)/. Post-order fold.
foldr :: (a -> b -> b) -> b -> MultiSet a -> b
foldr f z = Map.foldrWithKey repF z . unMS
 where repF a 1 b = f a b
       repF a n b = repF a (n - 1) (f a b)

-- | /O(n)/. Fold over the elements of a multiset with their occurences.
foldOccur :: (a -> Occur -> b -> b) -> b -> MultiSet a -> b
foldOccur f z = Map.foldrWithKey f z . unMS

{--------------------------------------------------------------------
  List variations 
--------------------------------------------------------------------}
-- | /O(t)/. The elements of a multiset.
elems :: MultiSet a -> [a]
elems = toList

-- | /O(n)/. The distinct elements of a multiset, each element occurs only once in the list.
--
-- > distinctElems = map fst . toOccurList
distinctElems :: MultiSet a -> [a]
distinctElems = Map.keys . unMS

{--------------------------------------------------------------------
  Lists 
--------------------------------------------------------------------}
-- | /O(t)/. Convert the multiset to a list of elements.
toList :: MultiSet a -> [a]
toList = toAscList

-- | /O(t)/. Convert the multiset to an ascending list of elements.
toAscList :: MultiSet a -> [a]
toAscList = foldr (:) []

-- | /O(t*log t)/. Create a multiset from a list of elements.
fromList :: Ord a => [a] -> MultiSet a 
fromList xs = fromOccurList $ zip xs (repeat 1)

-- | /O(t)/. Build a multiset from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: Eq a => [a] -> MultiSet a 
fromAscList xs = fromAscOccurList $ zip xs (repeat 1)

-- | /O(n)/. Build a multiset from an ascending list of distinct elements in linear time.
-- /The precondition (input list is strictly ascending) is not checked./
fromDistinctAscList :: [a] -> MultiSet a 
fromDistinctAscList xs = fromDistinctAscOccurList $ zip xs (repeat 1)

{--------------------------------------------------------------------
  Occurence lists 
--------------------------------------------------------------------}

-- | /O(n)/. Convert the multiset to a list of element\/occurence pairs.
toOccurList :: MultiSet a -> [(a,Occur)]
toOccurList = toAscOccurList

-- | /O(n)/. Convert the multiset to an ascending list of element\/occurence pairs.
toAscOccurList :: MultiSet a -> [(a,Occur)]
toAscOccurList = Map.toAscList . unMS


-- | /O(n*log n)/. Create a multiset from a list of element\/occurence pairs.
fromOccurList :: Ord a => [(a,Occur)] -> MultiSet a 
fromOccurList = MS . Map.fromListWith (+)

-- | /O(n)/. Build a multiset from an ascending list of element\/occurence pairs in linear time.
-- /The precondition (input list is ascending) is not checked./
fromAscOccurList :: Eq a => [(a,Occur)] -> MultiSet a 
fromAscOccurList = MS . Map.fromAscListWith (+)

-- | /O(n)/. Build a multiset from an ascending list of elements\/occurence pairs where each elements appears only once.
-- /The precondition (input list is strictly ascending) is not checked./
fromDistinctAscOccurList :: [(a,Occur)] -> MultiSet a 
fromDistinctAscOccurList = MS . Map.fromDistinctAscList

{--------------------------------------------------------------------
  Map
--------------------------------------------------------------------}

-- | /O(1)/. Convert a multiset to a 'Map' from elements to number of occurrences.
toMap :: MultiSet a -> Map a Occur
toMap = unMS

-- | /O(n)/. Convert a 'Map' from elements to occurrences to a multiset.
fromMap :: Ord a => Map a Occur -> MultiSet a
fromMap = MS . Map.filter (>0)

-- | /O(1)/. Convert a 'Map' from elements to occurrences to a multiset.
-- Assumes that the 'Map' contains only values larger than one.
-- /The precondition (all elements > 1) is not checked./
fromOccurMap :: Map a Occur -> MultiSet a
fromOccurMap = MS

{--------------------------------------------------------------------
  Set
--------------------------------------------------------------------}

-- | /O(n)/. Convert a multiset to a 'Set', removing duplicates.
toSet :: MultiSet a -> Set a
toSet = Map.keysSet . unMS

-- | /O(n)/. Convert a 'Set' to a multiset.
fromSet :: Set a -> MultiSet a
fromSet = fromDistinctAscList . Set.toAscList

{--------------------------------------------------------------------
  Instances 
--------------------------------------------------------------------}

instance Eq a => Eq (MultiSet a) where
  m1 == m2  =  unMS m1 == unMS m2

instance Ord a => Ord (MultiSet a) where
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

instance Show a => Show (MultiSet a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromOccurList " . shows (toOccurList xs)

{--------------------------------------------------------------------
  Read
--------------------------------------------------------------------}
instance (Read a, Ord a) => Read (MultiSet a) where
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
INSTANCE_TYPEABLE1(MultiSet,multiSetTc,"MultiSet")

{--------------------------------------------------------------------
  Split
--------------------------------------------------------------------}

-- | /O(log n)/. The expression (@'split' x set@) is a pair @(set1,set2)@
-- where all elements in @set1@ are lower than @x@ and all elements in
-- @set2@ larger than @x@. @x@ is not found in neither @set1@ nor @set2@.
split :: Ord a => a -> MultiSet a -> (MultiSet a,MultiSet a)
split a = (\(x,y) -> (MS x, MS y)) . Map.split a . unMS

-- | /O(log n)/. Performs a 'split' but also returns the number of
-- occurences of the pivot element in the original set.
splitOccur :: Ord a => a -> MultiSet a -> (MultiSet a,Occur,MultiSet a)
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
showTree :: Show a => MultiSet a -> String
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
showTreeWith :: Show a => Bool -> Bool -> MultiSet a -> String
showTreeWith hang wide = Map.showTreeWith s hang wide . unMS
  where s a n = showChar '(' . shows n . showString "*)" . shows a $ ""

{--------------------------------------------------------------------
  Assertions
--------------------------------------------------------------------}
-- | /O(n)/. Test if the internal multiset structure is valid.
valid :: Ord a => MultiSet a -> Bool
valid = Map.valid . unMS

{-
{--------------------------------------------------------------------
  Testing
--------------------------------------------------------------------}
testTree :: [Int] -> MultiSet Int
testTree xs   = fromList xs
test1 = testTree [1..20]
test2 = testTree [30,29..10]
test3 = testTree [1,4,6,89,2323,53,43,234,5,79,12,9,24,9,8,423,8,42,4,8,9,3]

{--------------------------------------------------------------------
  QuickCheck
--------------------------------------------------------------------}
qcheck prop
  = check config prop
  where
    config = Config
      { configMaxTest = 500
      , configMaxFail = 5000
      , configSize    = \n -> (div n 2 + 3)
      , configEvery   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ]
      }


{--------------------------------------------------------------------
  Arbitrary, reasonably balanced trees
--------------------------------------------------------------------}
instance (Enum a) => Arbitrary (MultiSet a) where
  arbitrary = fromMap `fmap` arbitrary

{--------------------------------------------------------------------
  Valid tree's
--------------------------------------------------------------------}
forValid :: (Enum a,Show a,Testable b) => (MultiSet a -> b) -> Property
forValid f
  = forAll arbitrary $ \t -> 
--    classify (balanced t) "balanced" $
    classify (size t == 0) "empty" $
    classify (size t > 0  && size t <= 10) "small" $
    classify (size t > 10 && size t <= 64) "medium" $
    classify (size t > 64) "large" $
    balanced t ==> f t

forValidIntTree :: Testable a => (MultiSet Int -> a) -> Property
forValidIntTree f
  = forValid f

forValidUnitTree :: Testable a => (MultiSet Int -> a) -> Property
forValidUnitTree f
  = forValid f


prop_Valid 
  = forValidUnitTree $ \t -> valid t

{--------------------------------------------------------------------
  Single, Insert, Delete
--------------------------------------------------------------------}
prop_Single :: Int -> Bool
prop_Single x
  = (insert x empty == singleton x)

prop_InsertValid :: Int -> Property
prop_InsertValid k
  = forValidUnitTree $ \t -> valid (insert k t)

prop_InsertDelete :: Int -> MultiSet Int -> Property
prop_InsertDelete k t
  = not (member k t) ==> delete k (insert k t) == t

prop_InsertOne :: Int -> MultiSet Int -> Bool
prop_InsertOne x t
  = (insertMany x 1 empty == singleton x)

prop_DeleteValid :: Int -> Property
prop_DeleteValid k
  = forValidUnitTree $ \t -> 
    valid (delete k (insert k t))

{--------------------------------------------------------------------
  Balance
--------------------------------------------------------------------}
prop_Join :: Int -> Property 
prop_Join x
  = forValidUnitTree $ \t ->
    let (l,r) = split x t
    in valid (join x l r)

prop_Merge :: Int -> Property 
prop_Merge x
  = forValidUnitTree $ \t ->
    let (l,r) = split x t
    in valid (merge l r)


{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
prop_UnionValid :: Property
prop_UnionValid
  = forValidUnitTree $ \t1 ->
    forValidUnitTree $ \t2 ->
    valid (union t1 t2)

prop_UnionInsert :: Int -> Set Int -> Bool
prop_UnionInsert x t
  = union t (singleton x) == insert x t

prop_UnionAssoc :: Set Int -> Set Int -> Set Int -> Bool
prop_UnionAssoc t1 t2 t3
  = union t1 (union t2 t3) == union (union t1 t2) t3

prop_UnionComm :: Set Int -> Set Int -> Bool
prop_UnionComm t1 t2
  = (union t1 t2 == union t2 t1)


prop_DiffValid
  = forValidUnitTree $ \t1 ->
    forValidUnitTree $ \t2 ->
    valid (difference t1 t2)

prop_Diff :: [Int] -> [Int] -> Bool
prop_Diff xs ys
  =  toAscList (difference (fromList xs) (fromList ys))
    == List.sort ((List.\\) (nub xs)  (nub ys))

prop_IntValid
  = forValidUnitTree $ \t1 ->
    forValidUnitTree $ \t2 ->
    valid (intersection t1 t2)

prop_Int :: [Int] -> [Int] -> Bool
prop_Int xs ys
  =  toAscList (intersection (fromList xs) (fromList ys))
    == List.sort (nub ((List.intersect) (xs)  (ys)))

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
prop_Ordered
  = forAll (choose (5,100)) $ \n ->
    let xs = [0..n::Int]
    in fromAscList xs == fromList xs

prop_List :: [Int] -> Bool
prop_List xs
  = (sort (nub xs) == toList (fromList xs))
-}
