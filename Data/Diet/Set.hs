{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyDataDecls      #-}
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
 ) where

import           Data.Diet.Internal.Debug
import           Data.Diet.Internal.Interval.Discrete (Interval (..))
import qualified Data.Diet.Internal.Interval.Discrete as I
import           Data.Diet.Internal.Nat
import qualified Data.Diet.Internal.Nat               as N
import           Data.Foldable                        (Foldable)
import           Data.List                            (sort)
import qualified Data.List                            as L
import           Data.Monoid
import qualified Data.Semigroup                       as S
import qualified GHC.Exts                             as GHCExts
import           Prelude                              hiding (null)

select :: Ord a => a -> a -> p -> p -> p -> p
select x y lt eq gt
  = case compare x y of { LT -> lt; EQ -> eq; GT -> gt }

select' :: Ord a => a -> a -> a -> p -> p -> p -> p -> p -> p
select' x y z xlty xeqy xbtw xeqz xgtz
  = select x y xlty xeqy (select x z xbtw xeqz xgtz)

d2 :: Diet n a -> Interval a -> Diet n a -> Diet (S n) a
d2 a b c     = BR (D2 a b c)

d3 :: Diet n a -> Interval a -> Diet n a -> Interval a -> Diet n a -> Diet (S n) a
d3 a b c d e = BR (D3 a b c d e)

data N (n :: Nat) a
  = D2 (Diet n a) (Interval a) (Diet n a)
  | D3 (Diet n a) (Interval a) (Diet n a) (Interval a) (Diet n a)

data Diet (n :: Nat) a where
  BR :: N n a -> Diet (S n) a
  LF :: Diet Z a

data Set a where
  Set :: Diet n a -> Set a

instance (Eq a) => Eq (Set a) where
  t == t' = (size t == size t') && (toList t == toList t')

instance (Ord a) => Ord (Set a) where
  compare t t' = compare (toList t) (toList t')

instance (Ord a) => Monoid (Set a) where
  mempty = empty
  mappend = union

instance (Ord a) => S.Semigroup (Set a) where
  (<>) = union

instance (Ord a) => GHCExts.IsList (Set a) where
  type Item (Set a) = Interval a
  fromList = fromList
  toList = toList

type Keep d n a = Diet n a -> d
type Push d n a = Diet n a -> Interval a -> Diet n a -> d

insert :: forall a. Ord a => Interval a -> Set a -> Set a
insert x (Set set) = ins set Set (\a b c -> Set (d2 a b c))
  where
    ins :: forall n d. Diet n a -> Keep d n a -> Push d n a -> d
    ins LF     = \keep push -> push LF x LF

    ins (BR n) = i n
      where
        i :: forall p m. (S p ~ m) => N p a -> Keep d m a -> Push d m a -> d
        i (D3 a b c d e) keep push = select' x b d xltb xeqb xbtw xeqd xgtd
          where
            xltb = ins a (\k -> keep (d3 k b c d e)) (\p q r -> push (d2 p q r) b (d2 c d e))
            xbtw = ins c (\k -> keep (d3 a b k d e)) (\p q r -> push (d2 a b p) q (d2 r d e))
            xgtd = ins e (\k -> keep (d3 a b c d k)) (\p q r -> push (d2 a b c) d (d2 p q r))
            xeqb = keep (d3 a x c d e)
            xeqd = keep (d3 a x c d e)
        i (D2 a b c) keep push = select x b xltb xeqb xgtb
          where
            xltb = ins a (\k -> keep (d2 k b c)) (\p q r -> keep (d3 p q r b c))
            xgtb = ins c (\k -> keep (d2 a b k)) (\p q r -> keep (d3 a b p q r))
            xeqb = keep (d2 a x c)

type Pull d n a = Shrunk n a -> d

data Shrunk (n :: Nat) a where
  H :: Diet n a -> Shrunk (S n) a

delete :: forall a. Ord a => Interval a -> Set a -> Set a
delete x (Set set) = search set Set shrink
  where
    shrink :: forall n. Shrunk n a -> Set a
    shrink (H d) = Set d

    search :: forall n d. Diet n a -> Keep d n a -> Pull d n a -> d
    search LF keep pull = keep LF

    search (BR (D2 a b c)) keep pull = select x b xltb xeqb xgtb
      where
        xltb, xeqb, xgtb :: d
        xltb = search a (\k -> keep (d2 k b c)) (\p -> mrgl p b c)
        xgtb = search c (\k -> keep (d2 a b k)) (\p -> mrg2r keep pull a b p)
        xeqb = replace a (\k r -> keep (d2 k r c)) (\p r -> mrgl p r c) (pull (H a))

        mrgl :: forall p. (S p ~ n) => Shrunk p a -> Interval a -> Diet p a -> d
        mrgl (H a) b (BR (D2 c d e))     = pull (H (d3 a b c d e))
        mrgl (H a) b (BR (D3 c d e f g)) = keep (d2 (d2 a b c) d (d2 e f g))

    search (BR (D3 a b c d e)) keep pull = select' x b d xltb xeqb xbtw xeqd xgtd
      where
        xltb, xeqb, xbtw, xeqd, xgtd :: d
        xltb = search a (\k -> keep (d3 k b c d e)) (\p -> mrgl p b c d e)
        xbtw = search c (\k -> keep (d3 a b k d e)) (\p -> mrgm a b p d e)
        xgtd = search e (\k -> keep (d3 a b c d k)) (\p -> mrg3r keep a b c d p)
        xeqb = replace a (\k r -> keep (d3 k r c d e)) (\p r -> mrgl p r c d e) (keep (d2 c d e))
        xeqd = replace c (\k r -> keep (d3 a b k r e)) (\p r -> mrgm a b p r e) (keep (d2 a b c))

        mrgl (H a) b (BR (D3 c d e f g)) h i = keep (d3 (d2 a b c) d (d2 e f g) h i)
        mrgl (H a) b (BR (D2 c d e)) f g     = keep (d2 (d3 a b c d e) f g)

        mrgm a b (H c) d (BR (D3 e f g h i)) = keep (d3 a b (d2 c d e) f (d2 g h i))
        mrgm a b (H c) d (BR (D2 e f g)) = keep (d2 a b (d3 c d e f g))

    replace :: forall n d. Diet n a -> Keep (Interval a -> d) n a -> Pull (Interval a -> d) n a -> d -> d
    replace LF keep pull leaf = leaf

    replace (BR (D2 a b c)) keep pull leaf
      = replace c (\k -> keep (d2 a b k)) (\p -> mrg2r keep pull a b p) (pull (H a) b)

    mrg2r :: forall p d. Keep d (S p) a -> Pull d (S p) a -> Diet p a -> Interval a -> Shrunk p a -> d
    mrg2r keep pull (BR (D2 a b c)) d (H e)     = pull (H (d3 a b c d e))
    mrg2r keep pull (BR (D3 a b c d e)) f (H g) = keep (d2 (d2 a b c) d (d2 e f g))

    mrg3r :: forall p d. Keep d (S p) a -> Diet p a -> Interval a -> Diet p a -> Interval a -> Shrunk p a -> d
    mrg3r keep a b (BR (D3 c d e f g)) h (H i) = keep (d3 a b (d2 c d e) f (d2 g h i))
    mrg3r keep a b (BR (D2 c d e)) f (H g) = keep (d2 a b (d3 c d e f g))

union :: forall a. Ord a => Set a -> Set a -> Set a
union t t'
  | t < t' = onion t' t
  | otherwise = onion t t'
  where
    onion :: Set a -> Set a -> Set a
    onion u v = L.foldl' (flip insert) u (toList v)

empty :: Set a
empty = Set LF

singleton :: (Ord a) => Interval a -> Set a
singleton x = insert x empty

null :: Set a -> Bool
null (Set LF) = True
null _        = False

size :: Set a -> Nat
size = L.foldl' (\c _ -> c N.+ (S Z)) Z

member :: forall a. Ord a => Interval a -> Set a -> Bool
member x (Set set) = mem set
  where
    mem :: Diet n a -> Bool
    mem (BR (D2 a b c))     = select x b (mem a) True (mem c)
    mem (BR (D3 a b c d e)) = select' x b d (mem a) True (mem c) True (mem e)
    mem LF                  = False

notMember :: forall a. Ord a => Interval a -> Set a -> Bool
notMember x s = not $ member x s

fromList :: forall a. Ord a => [Interval a] -> Set a
fromList = L.foldl' (flip insert) empty

toList :: Set a -> [Interval a]
toList = foldr (\x y -> (pure x) : y) []

instance Foldable Set where
  foldMap = hole

instance Show a => Show (Set a) where
  showsPrec n d = showParen (n > 10) $ showString "fromList " . shows (toList d)
