{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyDataDecls      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Diet.Internal.Nat
  ( Nat(..)
  , (+)
  ) where

import           Data.Monoid
import qualified Data.Semigroup as S
import           Prelude        hiding ((+))

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
