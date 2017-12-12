{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad           (foldM)
import           Data.Diet.Set           (Set (..))
import qualified Data.Diet.Set           as DS
import           Data.Foldable           (foldr)
import           Data.Functor.Identity
import           Data.Interval.Discrete  (Interval (..))
import qualified Data.Interval.Discrete  as I
import           Data.Proxy
import           Data.Semigroup          (Semigroup (..))
import           GHC.Exts                (IsList(..))
import           Test.QuickCheck
import           Test.QuickCheck.Classes as QC

instance Arbitrary a => Arbitrary (Interval a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    pure $ I a b

instance (Enum a, Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = do
    (i :: [Interval a]) <- arbitrary
    pure $ foldr DS.insert DS.empty i

-- p :: (Proxy :: Proxy (Type))
typeclassProps :: (Ord a, IsList a, Show (Item a), Arbitrary (Item a), Semigroup a, Eq a, Monoid a, Show a, Arbitrary a) => Proxy a -> [Laws]
typeclassProps p =
  [ QC.semigroupLaws p
  , QC.eqLaws p
  , QC.commutativeMonoidLaws p
  , ordLaws p
  , isListLaws p 
  --, foldableLaws
  ]

main :: IO ()
main = do
  foldMapM lawsCheck (typeclassProps (Proxy :: Proxy (Set Int)))

foldMapM :: (Foldable t, Monoid b, Monad m) => (a -> m b) -> t a -> m b
foldMapM f = foldM (\b a -> fmap (mappend b) (f a)) mempty

