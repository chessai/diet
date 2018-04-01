{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-unticked-promoted-constructors #-}

import Data.Interval (Interval(..))
import Data.Diet.Internal.Nat
--import Data.Kind

import Prelude (Ord(..), Ordering(..))

select :: Ord a => Interval a -> Interval a -> p -> p -> p -> p
select x y lt eq gt
  = case compare x y of { LT -> lt; EQ -> eq; GT -> gt }

select' :: Ord a => Interval a -> Interval a -> Interval a -> p -> p -> p -> p -> p -> p
select' x y z xlty xeqy xbtw xeqz xgtz
  = select x y xlty xeqy (select x z xbtw xeqz xgtz)

data N (n :: Nat) a
  = D2 (T n a) (Interval a) (T n a)
  | D3 (T n a) (Interval a) (T n a) (Interval a) (T n a)

data T (n :: Nat) a where
  BR :: N n a -> T (S n) a
  LF :: T Z a

data Tree a where
  Tree :: T n a -> Tree a

height :: T n a -> SNat n
height LF     = SZ
height (BR n) =
  case n of
    D2 t _ _ -> SS (height t)
    D3 t _ _ _ _ -> SS (height t)

--natDiff :: forall (n :: Nat) (m :: Nat). SNat n -> SNat m -> Either (Gte n m) (Gte m n)
--natDiff n m = go SZ n AdditionBase m AdditionBase

--cmp :: forall (n :: Nat) (m :: Nat). n -> m -> Either (Gte n m) (Gte m n)
--cmp Z Z = GteEq
--cmp Z _ = Lt
--cmp _ Z = Gt
--cmp (S n) (S m) = cmp n m

--compareHeight :: T n a -> T m a -> Either (Gte n m) (Gte m n)
--compareHeight a b = natDiff (height a) (height b)

type Keep t n a = T n a -> t
type Push t n a = T n a -> a -> T n a -> t


