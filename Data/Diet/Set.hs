{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyDataDecls      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

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
  ) where

import           Control.Applicative (Applicative (..), (<$>), (<*>))
import           Data.Foldable       (Foldable, toList)
import           Data.List           (sort)
import qualified Data.List           as L
import           Data.Monoid
import qualified Data.Semigroup      as S
import qualified GHC.Exts            as GHCExts
import           Prelude             hiding (null, (+))

select1 :: Ord a => a -> a -> p -> p -> p -> p
select1 x y lt eq gt
  = case compare x y of { LT -> lt; EQ -> eq; GT -> gt }

select2 :: Ord a => a -> a -> a -> p -> p -> p -> p -> p -> p
select2 x y z xlty xeqy xbtw xeqz xgtz
  = select1 x y xlty xeqy (select1 x z xbtw xeqz xgtz)

t1 :: T n a -> a -> T n a -> T (S n) a
t1 a b c = BR (T1 a b c)

t2 :: T n a -> a -> T n a -> a -> T n a -> T (S n) a
t2 a b c d e = BR (T2 a b c d e)

data Foo = Foo

data Nat = Z | S Nat | M | P
  deriving (Eq, Show)

infixl 6 +

(+) :: Nat -> Nat -> Nat
Z + m = m
(S n) + m = S (n + m)

instance S.Semigroup Nat where
  (<>) = (+)

instance Monoid Nat where
  mempty = Z
  mappend = (+)

data N (n :: Nat) a
  = T1 (T n a) a (T n a)
  | T2 (T n a) a (T n a) a (T n a)

data T (n :: Nat) a where
  BR :: N n a -> T (S n) a
  LF :: T Z a

data Set a where
  Set :: T n a -> Set a

instance (Eq a)  => Eq  (Set a) where
  t == t' = (size t == size t') && (toList t == toList t')

instance (Ord a) => Ord (Set a) where
  compare t t' = compare (toList t) (toList t')

instance (Ord a) => Monoid (Set a) where
  mempty = empty
  mappend = union

instance (Ord a) => S.Semigroup (Set a) where
  (<>) = union

instance (Ord a) => GHCExts.IsList (Set a) where
  type Item (Set a) = a
  fromList = fromList
  toList   = toList

size :: Set a -> Nat
size = L.foldl' (\c _ -> c + (S Z)) Z

-- This implementation is subpar. Currently it only
-- ensures that the smaller of the two Sets is
-- garbage collected, which will give at least some
-- performance boost.
union :: forall a. Ord a => Set a -> Set a -> Set a
union t t'
  | t < t'    = onion t' t
  | otherwise = onion t t'
  where
    onion :: Set a -> Set a -> Set a
    onion u v = L.foldl' (flip insert) u (toList v)

type Keep t n a = T n a -> t
type Push t n a = T n a -> a -> T n a -> t

insert :: forall a. Ord a => a -> Set a -> Set a
insert x (Set tree) = ins tree Set (\a b c -> Set (t1 a b c))
  where
    ins :: forall n t. T n a -> Keep t n a -> Push t n a -> t
    ins LF = \keep push -> push LF x LF

    ins (BR n) = i n
      where
        i :: forall p m. (S p ~ m) => N p a -> Keep t m a -> Push t m a -> t
        i (T2 a b c d e) keep push = select2 x b d xltb xeqb xbtw xeqd xgtd
          where
            xltb = ins a (\k -> keep (t2 k b c d e)) (\p q r -> push (t1 p q r) b (t1 c d e))
            xbtw = ins c (\k -> keep (t2 a b k d e)) (\p q r -> push (t1 a b p) q (t1 r d e))
            xgtd = ins e (\k -> keep (t2 a b c d k)) (\p q r -> push (t1 a b c) d (t1 p q r))
            xeqb = keep (t2 a x c d e)
            xeqd = keep (t2 a b c x e)

        i (T1 a b c) keep push = select1 x b xltb xeqb xgtb
          where
            xltb = ins a (\k -> keep (t1 k b c)) (\p q r -> keep (t2 p q r b c))
            xgtb = ins c (\k -> keep (t1 a b k)) (\p q r -> keep (t2 a b p q r))
            xeqb = keep (t1 a x c)

type Pull t n a = Shrunk n a -> t

data Shrunk (n :: Nat) a where
  H :: T n a -> Shrunk (S n) a

delete :: forall a. Ord a => a -> Set a -> Set a
delete x (Set tree) = search tree Set shrink
  where
    shrink :: forall n. Shrunk n a -> Set a
    shrink (H t) = Set t

    search :: forall n t. T n a -> Keep t n a -> Pull t n a -> t
    search LF keep pull = keep LF

    search (BR (T1 a b c)) keep pull = select1 x b xltb xeqb xgtb
      where
        xltb, xeqb, xgtb :: t
        xltb = search a (\k -> keep (t1 k b c)) (\p -> mrgl p b c)
        xgtb = search c (\k -> keep (t1 a b k)) (\p -> mrg2r keep pull a b p)
        xeqb = repl a (\k r -> keep (t1 k r c)) (\p r -> mrgl p r c) (pull (H a))

        mrgl :: forall p. (S p ~ n) => Shrunk p a -> a -> T p a -> t
        mrgl (H a) b (BR (T1 c d e))     = pull (H (t2 a b c d e))
        mrgl (H a) b (BR (T2 c d e f g)) = keep (t1 (t1 a b c) d (t1 e f g))

    search (BR (T2 a b c d e)) keep pull = select2 x b d xltb xeqb xbtw xeqd xgtd
      where
        xltb = search a (\k -> keep (t2 k b c d e)) (\p -> mrgl p b c d e)
        xbtw = search c (\k -> keep (t2 a b k d e)) (\p -> mrgm a b p d e)
        xgtd = search e (\k -> keep (t2 a b c d k)) (\p -> mrg3r keep a b c d p)
        xeqb = repl a (\k r -> keep (t2 k r c d e)) (\p r -> mrgl p r c d e) (keep (t1 c d e))
        xeqd = repl c (\k r -> keep (t2 a b k r e)) (\p r -> mrgm a b p r e) (keep (t1 a b c))

        mrgl (H a) b (BR (T2 c d e f g)) h i = keep (t2 (t1 a b c) d (t1 e f g) h i)
        mrgl (H a) b (BR (T1 c d e)) f g = keep (t1 (t2 a b c d e) f g)

        mrgm a b (H c) d (BR (T2 e f g h i)) = keep (t2 a b (t1 c d e) f (t1 g h i))
        mrgm a b (H c) d (BR (T1 e f g)) = keep (t1 a b (t2 c d e f g))

    repl :: forall n t. T n a -> Keep (a -> t) n a -> Pull (a -> t) n a -> t -> t
    repl LF keep pull leaf = leaf

    repl (BR (T1 a b c)) keep pull leaf
      = repl c (\k -> keep (t1 a b k)) (\p -> mrg2r keep pull a b p) (pull (H a) b)

    repl (BR (T2 a b c d e)) keep pull leaf =
      repl e (\k -> keep (t2 a b c d k)) (\p -> mrg3r keep a b c d p) (keep (t1 a b c) d)

    mrg2r :: forall p t. Keep t (S p) a -> Pull t (S p) a -> T p a -> a -> Shrunk p a -> t
    mrg2r keep pull (BR (T1 a b c)) d (H e) = pull (H (t2 a b c d e))
    mrg2r keep pull (BR (T2 a b c d e)) f (H g) = keep (t1 (t1 a b c) d (t1 e f g))

    mrg3r :: forall p t. Keep t (S p) a -> T p a -> a -> T p a -> a -> Shrunk p a -> t
    mrg3r keep a b (BR (T2 c d e f g)) h (H i) = keep (t2 a b (t1 c d e) f (t1 g h i))
    mrg3r keep a b (BR (T1 c d e)) f (H g) = keep (t1 a b (t2 c d e f g))

empty :: Set a
empty = Set LF

singleton :: (Ord a) => a -> Set a
singleton x = insert x empty

null :: Set a -> Bool
null (Set LF) = True
null _        = False

member :: forall a. Ord a => a -> Set a -> Bool
member x (Set tree) = mem tree
  where
    mem :: T n a -> Bool
    mem (BR (T1 a b c))     = select1 x b (mem a) True (mem c)
    mem (BR (T2 a b c d e)) = select2 x b d (mem a) True (mem c) True (mem e)
    mem LF                  = False

notMember :: forall a. Ord a => a -> Set a -> Bool
notMember x t = not $ member x t

fromList :: Ord a => [a] -> Set a
fromList = L.foldl' (flip insert) empty

instance Foldable Set where
  foldMap = foldm
    where
      foldm :: forall m a. Monoid m => (a -> m) -> Set a -> m
      foldm f (Set t) = fm t
        where
          fm :: forall n. T n a -> m
          fm (BR (T1 a b c))     = fm a <> f b <> fm c
          fm (BR (T2 a b c d e)) = fm a <> f b <> fm c <> f d <> fm e
          fm LF                  = mempty

instance Show a => Show (Set a) where
  showsPrec n t = showParen (n > 10) $ showString "fromList " . shows (toList t)
