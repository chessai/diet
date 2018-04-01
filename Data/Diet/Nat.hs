{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Diet.Nat
  ( Nat(..)
  , SNat
  , Gte
  , caseGte
  , natDiff
  , succSNat
  , zeroSNat
  ) where

import Data.Type.Equality
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

data Nat = Z | S Nat
  deriving (Eq, Show)

newtype SNat (n :: Nat) = SNat Int
newtype Gte (n :: Nat) (m :: Nat) = Gte Int

natDiff :: forall (n :: Nat) (m :: Nat). SNat n -> SNat m -> Either (Gte n m) (Gte m n)
natDiff (SNat n) (SNat m) = if n <= m
  then Right (Gte (m - n))
  else Left  (Gte (n - m))

zeroSNat :: SNat 'Z
zeroSNat = SNat 0

succSNat :: SNat n -> SNat ('S n)
succSNat (SNat n) = SNat (n + 1)

caseGte :: forall (n :: Nat) (m :: Nat) a.
     Gte n m
  -> ((n ~ m) => a)
  -> (forall (p :: Nat). ('S p ~ n) => Gte p m -> a) 
  -> a
caseGte (Gte d) a f = if d > 0
  then
    let gt :: forall (p :: Nat). ('S p ~ n) => Gte p m
        gt = Gte (d - 1)
     in case unsafeEquality (Proxy :: Proxy ('S p)) (Proxy :: Proxy n) of
          Refl -> f gt
  else case unsafeEquality (Proxy :: Proxy n) (Proxy :: Proxy m) of
    Refl -> a

unsafeEquality :: Proxy n -> Proxy m -> n :~: m
unsafeEquality _ _ = unsafeCoerce Refl
