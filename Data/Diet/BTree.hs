{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language NoImplicitPrelude #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

module Data.Diet.BTree where

import           Control.Applicative (Applicative(..), (<$>), (<*>), liftA3)
import           Data.Either
import           Data.Foldable (Foldable)
import           Data.Interval (Interval(..))
import qualified Data.Interval as I
import           Data.Semigroup ((<>))
import           Data.Monoid (Monoid(..))
import           Data.Semigroup (Semigroup)
import           Prelude (Enum(..), Functor(..), Ord(..), (&&), Int, flip)


data Leaf k v = L !(Interval k) v
  deriving (Eq)

data Map' k v where
  Empty :: Map k v
  LF    :: (Leaf k v) -> v -> Map k v
  BR    :: Size -> Interval k -> v -> Map k v

--data Map k v
--  = Empty
--  | Leaf !(Leaf k v) v
--  | BR {-# UNPACK #-} !Size !(Interval k) v !(Map k v)

-- Leaves store all the values.
-- Branches store an interval whose endpoints contain information about the subtrees.
--
--
data Map k v = LF !(Interval k) v
             | BR {-# UNPACK #-} !Size !(Interval k) v !(Map k v) !(Map k v)

type Size    = Int

empty :: (Enum k, Ord k, Monoid v) => Map k v
empty = LF mempty mempty

singleton :: Interval k -> v -> Map k v
singleton k v = BR 1 k v (LF k v) (LF mempty mempty)

map :: (a -> b) -> Map k a -> Map k b
map f = go where
  go (LF k v) = LF k (f v)
  go (BR sx kx x l r) = BR sx kx (f x) (go l) (go r)

instance Functor (Map k) where
  fmap = map

lookup :: (Enum k, Ord k, Monoid v) => k -> Map k v -> v
lookup k (LF i@(I.I a b) v) = if (k >= a && k <= b) then v else mempty 
lookup k (BR _ _ v l r) = (lookup k l) `mappend` (lookup k r) 

--nodeToInterval :: (Enum k, Ord k) => Map k v -> (Interval k, v)
--nodeToInterval (LF i v) = (i, v)
--nodeToInterval (BR _ i v _ _) = (i, v)

beyonce a b = a `I.overlaps` b && (I.inf a < I.inf b) && (I.sup a < I.sup b)

notBeyonce a b = beyonce b a
--a `I.overlaps` b && (I.inf a > I.inf b) && (I.sup a > I.sup b)

query :: (Enum k, Ord k, Monoid v) => Interval k -> Map k v -> v
query i (LF i' v)       = if i `I.overlaps` i' then v else mempty
query i@(I.I a b) (BR _ i'@(I.I x y) v l r) =
  if i `beyonce` i' 
    then v
    else v

toList :: (Enum k, Ord k) => Map k a -> [(Interval k, a)]
toList = toAscList

toAscList :: (Enum k, Ord k) => Map k a -> [(Interval k, a)]
toAscList = foldrWithKey (\k x xs -> (k,x):xs) []

toDescList :: (Enum k, Ord k) => Map k a -> [(Interval k, a)]
toDescList = foldlWithKey (\xs k x -> (k,x):xs) []

foldr :: (Enum k, Ord k) => (v -> b -> b) -> b -> Map k v -> b
foldr _ z (LF _ _) = z
foldr f z (BR _ _ v l r) = foldr f (f v (foldr f z r)) l

foldl :: (Enum k, Ord k) => (b -> v -> b) -> b -> Map k v -> b
foldl _ z (LF _ _) = z
foldl f z (BR _ _ v l r) = foldl f (f (foldl f z l) v) r

foldrWithKey :: (Enum k, Ord k) => (Interval k -> v -> a -> a) -> a -> Map k v -> a
foldrWithKey _ z (LF _ _) = z
foldrWithKey f z (BR _ k v l r) = foldrWithKey f (f k v (foldrWithKey f z r)) l

foldlWithKey :: (Enum k, Ord k) => (a -> Interval k -> v -> a) -> a -> Map k v -> a
foldlWithKey _ z (LF _ _) = z
foldlWithKey f z (BR _ k v l r) = foldlWithKey f (f (foldlWithKey f z l) k v) r

foldr' :: (Enum k, Ord k) => (a -> b -> b) -> b -> Map k a -> b
foldr' _ !z (LF _ _) = z
foldr' f !z (BR _ _ v l r) = foldr' f (f v (foldr' f z r)) l

foldl' :: (Enum k, Ord k) => (b -> a -> b) -> b -> Map k a -> b
foldl' _ !z (LF _ _) = z
foldl' f !z (BR _ k v l r) = foldl' f (f (foldl' f z l) v) r

foldrWithKey' :: (Enum k, Ord k) => (Interval k -> v -> a -> a) -> a -> Map k v -> a
foldrWithKey' _ !z (LF _ _) = z
foldrWithKey' f !z (BR _ k v l r) = foldrWithKey' f (f k v (foldrWithKey' f z r)) l

foldlWithKey' :: (Enum k, Ord k) => (a -> Interval k -> v -> a) -> a -> Map k v -> a
foldlWithKey' _ !z (LF _ _) = z
foldlWithKey' f !z (BR _ k v l r) = foldlWithKey' f (f (foldlWithKey' f z l) k v) r

foldMapWithKey :: Monoid m => (Interval k -> v -> m) -> Map k v -> m
foldMapWithKey f = go
  where
    go (LF _ _) = mempty
    go (BR 1 k v _ _) = f k v
    go (BR _ k v l r) = go l `mappend` (f k v `mappend` go r)
