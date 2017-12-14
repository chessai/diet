{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Diet.Internal.Set where

import           Data.Diet.Internal.Debug
import           Data.Interval.Discrete (Interval (..), IOrdering(..))
import qualified Data.Interval.Discrete as I
import           Data.Diet.Internal.Nat
import qualified Data.Diet.Internal.Nat as N
import           Data.Monoid

select :: (Enum a, Ord a) => Interval a -> Interval a -> p -> p -> p -> p -> p
select x y lt eq gt ov
  = case I.cmp x y of { L -> lt; E -> eq; G -> gt; O -> ov }

select' :: (Enum a, Ord a) => Interval a -> Interval a -> Interval a -> p -> p -> p -> p -> p -> p -> p -> p
select' x y z xlty xeqy xovy xbtw xeqz xgtz xovz
  = select x y xlty xeqy xovy (select x z xbtw xeqz xgtz xovz)

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

type Keep d n a = Diet n a -> d
type Push d n a = Diet n a -> Interval a -> Diet n a -> d
type Pull d n a = Shrunk n a -> d

data Shrunk (n :: Nat) a where
  H :: Diet n a -> Shrunk (S n) a

search :: forall n d a. (Enum a, Ord a) => Interval a -> Diet n a -> Keep d n a -> Pull d n a -> d
search x LF keep pull = keep LF
search x (BR (D2 a b c)) keep pull = select x b xltb xeqb xgtb xovb
  where
    xltb, xeqb, xgtb, xovb :: d
    xltb = search x a (\k -> keep (d2 k b c)) (\p -> mrgl p b c)
    xgtb = search x c (\k -> keep (d2 a b k)) (\p -> mrg2r keep pull a b p)
    xeqb = replace x a (\k r -> keep (d2 k r c)) (\p r -> mrgl p r c) (pull (H a))
    xovb = undefined

    mrgl :: forall p. (S p ~ n) => Shrunk p a -> Interval a -> Diet p a -> d
    mrgl (H a) b (BR (D2 c d e)) = pull (H (d3 a b c d e))
    mrgl (H a) b (BR (D3 c d e f g)) = keep (d2 (d2 a b c) d (d2 e f g))
search x (BR (D3 a b c d e)) keep pull = select' x b d xltb xeqb xovb xbtw xeqd xgtd xovd
  where
    xltb, xeqb, xovb, xbtw, xeqd, xgtd, xovd :: d
    xltb = search x a (\k -> keep (d3 k b c d e)) (\p -> mrgl p b c d e)
    xbtw = search x c (\k -> keep (d3 a b k d e)) (\p -> mrgm a b p d e)
    xgtd = search x e (\k -> keep (d3 a b c d k)) (\p -> mrg3r keep a b c d p)
    xeqb = replace x a (\k r -> keep (d3 k r c d e)) (\p r -> mrgl p r c d e) (keep (d2 c d e))
    xeqd = replace x c (\k r -> keep (d3 a b k r e)) (\p r -> mrgm a b p r e) (keep (d2 a b c))
    xovb = undefined
    xovd = undefined

    mrgl (H a) b (BR (D3 c d e f g)) h i = keep (d3 (d2 a b c) d (d2 e f g) h i)
    mrgl (H a) b (BR (D2 c d e)) f g     = keep (d2 (d3 a b c d e) f g)

    mrgm a b (H c) d (BR (D3 e f g h i)) = keep (d3 a b (d2 c d e) f (d2 g h i))
    mrgm a b (H c) d (BR (D2 e f g)) = keep (d2 a b (d3 c d e f g))

replace :: forall n d a. (Enum a, Ord a) => Interval a -> Diet n a -> Keep (Interval a -> d) n a -> Pull (Interval a -> d) n a -> d -> d
replace x LF keep pull leaf = leaf
replace x (BR (D2 a b c)) keep pull leaf
  = replace x c (\k -> keep (d2 a b k)) (\p -> undefined) (pull (H a) b)

mrg2r :: forall p d a. (Enum a, Ord a) => Keep d (S p) a -> Pull d (S p) a -> Diet p a -> Interval a -> Shrunk p a -> d
mrg2r keep pull (BR (D2 a b c)) d (H e) = pull (H (d3 a b c d e))
mrg2r keep pull (BR (D3 a b c d e)) f (H g) = keep (d2 (d2 a b c) d (d2 e f g))

mrg3r :: forall p d a. (Enum a, Ord a) => Keep d (S p) a -> Diet p a -> Interval a -> Diet p a -> Interval a -> Shrunk p a -> d
mrg3r keep a b (BR (D3 c d e f g)) h (H i) = keep (d3 a b (d2 c d e) f (d2 g h i))
mrg3r keep a b (BR (D2 c d e)) f (H g) = keep (d2 a b (d3 c d e f g))

insert :: forall n d a. (Enum a, Ord a) => Interval a -> Diet n a -> Keep d n a -> Push d n a -> d
insert x LF = \keep push -> push LF x LF
insert x (BR n) = i n
  where
    i :: forall p m. (S p ~ m) => N p a -> Keep d m a -> Push d m a -> d
    i (D3 a b c d e) keep push = select' x b d xltb xeqb xovb xbtw xeqd xgtd xovd
      where
        xltb, xeqb, xovb, xbtw, xeqd, xgtd, xovd :: d
        xltb = insert x a (\k -> keep (d3 k b c d e)) (\p q r -> push (d2 p q r) b (d2 c d e)) 
        xbtw = insert x c (\k -> keep (d3 a b k d e)) (\p q r -> push (d2 a b p) q (d2 r d e))
        xgtd = insert x e (\k -> keep (d3 a b c d k)) (\p q r -> push (d2 a b c) d (d2 p q r)) 
        xeqb = keep (d3 a x c d e)
        xeqd = keep (d3 a b c x e)
        xovb = keep (d3 a (b <> x) c d e) 
        xovd = keep (d3 a b c (d <> x) e)
    i (D2 a b c) keep push = select x b xltb xeqb xgtb xovb
      where
        xltb, xeqb, xgtb, xovb :: d
        xltb = insert x a (\k -> keep (d2 k b c)) (\p q r -> keep (d3 p q r b c)) 
        xgtb = insert x c (\k -> keep (d2 a b k)) (\p q r -> keep (d3 a b p q r)) 
        xeqb = keep (d2 a x c)
        xovb = keep (d2 a (b <> x) c)

member :: (Enum a, Ord a) => Interval a -> Diet n a -> Bool
member x (BR (D2 a b c))     = select x b (member x a) True (member x c) True
member x (BR (D3 a b c d e)) = select' x b d (member x a) True True (member x c) True (member x e) True 
member x LF                  = False
