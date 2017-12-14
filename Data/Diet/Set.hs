{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Diet.Set
 ( -- * Diet type
   Set(..)

   -- * Construction
 , empty
 , singleton
 , insert
 , delete

   -- * Combine
 , union

   -- * Query
 , null
 , member
 , notMember
 , size

   -- * Conversion
 , toList
 , fromList
   
   -- * Folds
 , fold
 , foldMap
 , foldr
 , foldr'
 , foldl
 , foldl'
 ) where

import           Data.Diet.Internal.Debug
import           Data.Interval.Discrete   (Interval (..))
import qualified Data.Interval.Discrete   as I
import           Data.Diet.Internal.Nat
import qualified Data.Diet.Internal.Nat   as N
import           Data.Diet.Internal.Set   (Diet(..), N(..), Shrunk(..))
import qualified Data.Diet.Internal.Set   as S
import           Data.List                (sort)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Semigroup           as Semigroup
import qualified GHC.Exts                 as GHCExts
import           Prelude                  hiding (foldMap, foldl, foldl', foldr, null)

data Set a where
  Set :: Diet n a -> Set a

instance (Enum a, Eq a, Ord a) => Eq (Set a) where
  t == t' = toList t == toList t' -- && (size t == size t')

instance (Enum a, Eq a, Ord a) => Ord (Set a) where
  compare t t' = compare (toList t) (toList t')

instance (Enum a, Ord a) => Monoid (Set a) where
  mempty  = empty
  mappend = union

instance (Enum a, Ord a) => Semigroup.Semigroup (Set a) where
  (<>) = union

instance (Enum a, Ord a) => GHCExts.IsList (Set a) where
  type Item (Set a) = Interval a
  fromList = fromList
  toList = toList

insert :: forall a. (Enum a, Ord a) => Interval a -> Set a -> Set a
insert x s@(Set set) = case (I.valid x) of
  True  -> S.insert x set Set (\a b c -> Set (S.d2 a b c))
  False -> s

delete :: forall a. (Enum a, Ord a) => Interval a -> Set a -> Set a
delete x s@(Set set) = case (I.valid x) of
  True  -> S.search x set Set shrink
  False -> s
  where
    shrink :: forall n. Shrunk n a -> Set a
    shrink (H d) = Set d

-- This implementation is subpar. Currently it only
-- ensures that the smaller of the two Sets is
-- garbage collected, which will give at least some
-- performance boost.
union :: forall a. (Enum a, Ord a) => Set a -> Set a -> Set a
union t t' 
  | t < t' = onion t' t
  | otherwise = onion t t'
  where
    onion :: Set a -> Set a -> Set a
    onion u v = foldl' (flip insert) u v

empty :: Set a
empty = Set LF

singleton :: (Enum a, Ord a) => Interval a -> Set a
singleton x = Set (BR (D2 LF x LF))

null :: Set a -> Bool
null (Set LF) = True
null _        = False

-- not even that useful
size :: Set a -> Nat
size = foldl' (\c _ -> c N.+ (S Z)) Z 

member :: forall a. (Enum a, Ord a) => Interval a -> Set a -> Bool
member x (Set set) = S.member x set

notMember :: forall a. (Enum a, Ord a) => Interval a -> Set a -> Bool
notMember x s = not $ member x s

fromList :: forall a. (Enum a, Ord a) => [Interval a] -> Set a
fromList [] = Set LF
fromList (!x:xs) = insert x $ fromList xs

toList :: Set a -> [Interval a]
toList = foldMap (\x -> [x])

fold :: (Enum a, Monoid a, Ord a) => Set a -> Interval a
fold = foldMap id

foldMap :: forall m a. Monoid m => (Interval a -> m) -> Set a -> m
foldMap f (Set set) = fm set
  where
    fm :: forall n. Diet n a -> m
    fm (BR (D2 a b c))     = fm a <> f b <> fm c
    fm (BR (D3 a b c d e)) = fm a <> f b <> fm c <> f d <> fm e
    fm LF                  = mempty

-- | lazy foldr
foldr :: (Interval a -> b -> b) -> b -> Set a -> b
foldr f z s = appEndo (foldMap (Endo . f) s) z

-- | strict foldr
foldr' :: (Interval a -> b -> b) -> b -> Set a -> b
foldr' f !z s = foldr f z s

-- | lazy foldl
foldl :: (a -> Interval b -> a) -> a -> Set b -> a
foldl f z s = appEndo (getDual (foldMap (Dual . Endo . flip f) s)) z

-- | strict foldl
foldl' :: (a -> Interval b -> a) -> a -> Set b -> a
foldl' f !z s = foldl f z s 

instance Show a => Show (Set a) where
  showsPrec n d = showParen (n > 10) $ showString "fromList " . shows (toList d)
