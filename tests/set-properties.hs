{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
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
import           Test.QuickCheck
import           Test.QuickCheck.Classes as QC

instance Arbitrary a => Arbitrary (Interval a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    pure $ I a b

instance (Enum a, Ord a, Arbitrary a) => Arbitrary (Set (Interval a)) where
  arbitrary = do
    (i :: [Interval a]) <- arbitrary
    pure $ foldr DS.insert DS.empty i

-- p :: (Proxy :: Proxy (Type))
typeclassProps :: (Semigroup a, Eq a, Monoid a, Show a, Arbitrary a) => Proxy a -> [(String,Property)]
typeclassProps p = concat
  [ QC.semigroupProps p
  , QC.eqProps p
  , QC.communativeMonoidProps p
  ]

main :: IO ()
main = do
  foldMapM (quickCheck . snd) (typeclassProps (Proxy :: Proxy (Set (Interval Int))))

foldMapM :: (Foldable t, Monoid b, Monad m) => (a -> m b) -> t a -> m b
foldMapM f = foldM (\b a -> fmap (mappend b) (f a)) mempty

