{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Diet.Internal.Nat where

data Nat = Z | S Nat

infixl 6 +,-
infixl 7 *

type family (n :: Nat) + (m :: Nat) :: Nat where
  Z + m = m
  (S n) + m = S (n + m)

type family (n :: Nat) * (m :: Nat) :: Nat where
  Z * m = Z
  (S n) * m = (n * m) + m

type family (n :: Nat) - (m :: Nat) :: Nat where
  Z - m = Z
  (S n) - Z = S n
  (S n) - m = S (n - m)

type family Min (n :: Nat) (m :: Nat) :: Nat where
  Min Z Z = Z
  Min (S n) Z = Z
  Min (S n) (S m) = S (Min n m)

type family Max (n :: Nat) (m :: Nat) :: Nat where
  Max Z Z = Z
  Max (S n) Z = S n
  Max (S n) (S m) = S (Max n m)
