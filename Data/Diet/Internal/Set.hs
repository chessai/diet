{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Diet.Internal.Set where

import           Data.Interval (Interval (..))
import qualified Data.Interval as I
import           Data.Diet.Internal.Nat
import qualified Data.Diet.Internal.Nat as N
import           Data.Monoid

import Prelude (otherwise, undefined)

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


