{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           System.Random           (mkStdGen)
import           System.Random.Shuffle   (shuffleM)
import           Control.Monad.Random    (evalRand)
import           Control.Exception       (Exception,toException)
import           Test.QuickCheck.Property (exception)

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
main = do
  props
  quickCheck prop_logarithmic
  quickCheck prop_collapse

props :: IO ()
props = lawsCheckMany allPropsApplied

prop_logarithmic :: Map Int Int -> Bool
prop_logarithmic m = (h <= DM.maxHeight n) && n <= DM.maxNodes h
  where
    n :: Int
    n = DM.size m
    h :: Int
    h = DM.height m

prop_collapse :: Property
prop_collapse = forAll (choose (0,1000000)) $ \seed -> do
  let elems0 = enumFromTo (1 :: Int) (12 :: Int)
  let elems1 = evalRand (shuffleM elems0) (mkStdGen seed)
  let sz = DM.size (DM.fromList (map (\x -> (I.singleton x,())) elems1))
  if sz == 1
    then property True
    else do
      let msg = "fromList " ++ show elems1 ++ " produces map of size " ++ show sz
      property $ exception msg (toException PropCollapseException)

data PropCollapseException = PropCollapseException
  deriving (Show,Eq)

instance Exception PropCollapseException

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
