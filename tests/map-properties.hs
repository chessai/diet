{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad           (foldM)
import           Data.Diet.Map           (Map (..))
import qualified Data.Diet.Map           as DM
import           Data.Foldable           (foldr)
import           Data.Functor.Identity
import           Data.Interval           (Interval (..))
import qualified Data.Interval           as I
import           Data.Proxy
import           Data.Semigroup          (Semigroup (..))
import           GHC.Exts                (IsList(..))
import           Test.QuickCheck
import           Test.QuickCheck.Classes as QC

instance Monoid Int where
  mempty  = 0
  mappend = (+)
  mconcat = sum

instance Arbitrary a => Arbitrary (Interval a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    pure $ I a b

instance (Enum k, Ord k, Enum v, Ord v, Arbitrary k, Arbitrary v) => Arbitrary (Map k v) where
  arbitrary = do
    (i :: [(Interval a, v)]) <- arbitrary
    pure $ DM.fromList i
  shrink s = map DM.fromList (shrink (DM.toList s))

main :: IO ()
main = lawsCheckMany allPropsApplied

typeclassProps :: (Ord a, IsList a, Show (Item a), Arbitrary (Item a), Eq a, Monoid a, Show a, Arbitrary a) => Proxy a -> [Laws]
typeclassProps p =
  [ QC.eqLaws p
  , QC.ordLaws p
  , QC.isListLaws p 
  , QC.commutativeMonoidLaws p 
  --, QC.foldableLaws p
  --, QC.functorLaws p
  ]

allPropsApplied :: [(String,[Laws])]
allPropsApplied =
  [ ("Diet Maps",typeclassProps (Proxy :: Proxy (Map Int Int)))
  ]
