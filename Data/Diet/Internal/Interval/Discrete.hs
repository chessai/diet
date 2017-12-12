{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE Rank2Types         #-}

module Data.Diet.Internal.Interval.Discrete
  ( Interval(..)
  , (...)
  , (+/-)
  , interval
  , valid
  , invalid
  , singleton
  , empty 
  , inf
  , sup
  , singular
  , width
  , map
  , toList
  , magnitude
  , mignitude
  , distance
  , inflate
  , deflate
  , symmetric
  , bisectIntegral
  , member
  , notMember
  , increasing
  , decreasing
  , hull
  , contains
  , isSubsetOf
  , adjacent
  , merge
  , (<!)
  , (<=!)
  , (==!)
  , (/=!)
  , (>!)
  , (>=!)
  , (<?)
  , (<=?)
  , (==?)
  , (/=?)
  , (>?)
  , (>=?)
  ) where

import           Control.Monad     (guard)
import           Data.Data
import           Data.Distributive
import           Data.Semigroup    (Semigroup (..))
import           GHC.Generics
import           Prelude           hiding (map)

data Interval a = I !a !a | Empty
  deriving (Eq, Ord, Data, Typeable, Generic, Generic1)

instance Ord a => Semigroup (Interval a) where
  (<>) = hull
  {-# INLINE (<>) #-}

instance Ord a => Monoid (Interval a) where
  mempty = empty
  mappend = (<>)

instance Functor Interval where
  fmap f (I a b) = I (f a) (f b)
  {-# INLINE fmap #-}

instance Foldable Interval where
  foldMap f (I a b) = f a `mappend` f b
  {-# INLINE foldMap #-}

instance Traversable Interval where
  traverse f (I a b) = I <$> f a <*> f b
  {-# INLINE traverse #-}

instance Applicative Interval where
  pure a = I a a
  {-# INLINE pure #-}
  I f g <*> I a b = I (f a) (g b)
  {-# INLINE (<*>) #-}

instance Monad Interval where
  return a = I a a
  {-# INLINE return #-}
  I a b >>= f = I a' b' where
    I a' _ = f a
    I _ b' = f b
  {-# INLINE (>>=) #-}

instance Distributive Interval where
  distribute f = fmap inf f ... fmap sup f
  {-# INLINE distribute #-}

instance Show a => Show (Interval a) where
  showsPrec n (I a b) =
    showParen (n > 3) $
      showsPrec 3 a .
      showString " ... " .
      showsPrec 3 b

infix  3 ...
infixl 6 +/-

(+/-) :: (Num a, Ord a) => a -> a -> Interval a
a +/- b = a - b ... a + b
{-# INLINE (+/-) #-}

(...) :: a -> a -> Interval a
(...) = I
{-# INLINE (...) #-}

interval :: Ord a => a -> a -> Interval a
interval a b
  | a <= b = I a b
  | otherwise = I b a
{-# INLINE interval #-}

empty :: Interval a
empty = Empty
{-# INLINE empty #-}

-- | test whether or not an Interval is valid
--
-- >>> valid (1 ... 5)
-- True
--
-- >>> valid (5 ... 1)
-- False
valid :: Ord a => Interval a -> Bool
valid x = inf x <= sup x
{-# INLINE valid #-}

-- | opposite of valid
-- >>> invalid (1 ... 5)
-- False
--
-- >>> invalid (5 ... 1)
-- True
invalid :: Ord a => Interval a -> Bool
invalid = not . valid
{-# INLINE invalid #-}

-- | A degenerate interval
--
-- >>> singleton 1
-- 1 ... 1
singleton :: a -> Interval a
singleton a = a ... a
{-# INLINE singleton #-}

-- | The infinimum (lower bound) of an interval
--
-- >>> inf (1 ... 20)
-- 1
inf :: Interval a -> a
inf (I a _) = a
{-# INLINE inf #-}

-- | The supremum (upper bound) of an interval
--
-- >>> sup (1 ... 20)
-- 20
sup :: Interval a -> a
sup (I _ b) = b
{-# INLINE sup #-}

-- | Is the interval degenerate?
--
-- >>> singular (singleton 1)
-- True
--
-- >>> singular (1 ... 20)
-- False
singular :: Ord a => Interval a -> Bool
singular x = valid x && inf x == sup x
{-# INLINE singular #-}

map :: (Enum a, Ord a, Ord b) => (a -> b) -> Interval a -> Interval b
map f i@(I x y)
  | singular i = singleton (f x)
  | otherwise  = (singleton (f x)) `hull` map f (I (succ x) y)

toList :: Enum a => Interval a -> [a]
toList (I a b) = [a..b]

-- | Calculate the width of an interval.
--
-- >>> width (1 ... 20)
-- 19
--
-- >>> width (singleton 1)
-- 0
width :: (Enum a, Num a) => Interval a -> a
width (I a b) = succ $ b - a
{-# INLINE width #-}

-- | Magnitude
--
-- >> magnitude (1 ... 20)
-- 20
--
-- >>> magnitude (-20 ... 10)
-- 20
--
-- >>> magnitude (singleton 5)
-- 5
magnitude :: (Num a, Ord a) => Interval a -> a
magnitude = sup . abs
{-# INLINE magnitude #-}

-- | \"mignitude\"
--
-- >>> mignitude (1 ... 20)
-- 1
--
-- >>> mignitude (-20 ... 10)
-- 0
--
-- >>> mignitude (singleton 5)
-- 5
mignitude :: (Num a, Ord a) => Interval a -> a
mignitude = inf . abs
{-# INLINE mignitude #-}

-- | Hausdorff distance between intervals.
--
-- >>> distance (1 ... 7) (6 ... 10)
-- 0
--
-- >>> distance (1 ... 7) (15 ... 24)
-- 8
--
-- >>> distance (1 ... 7) (-10 ... -2)
-- 3
distance :: (Num a, Ord a) => Interval a -> Interval a -> a
distance i1 i2 = mignitude (i1 - i2)

-- | Inflate an interval by enlarging it at both ends.
--
-- >>> inflate 3 (-1 ... 7)
-- -4 ... 10
--
-- >>> inflate (-2) (0 ... 4)
-- 2 ... 2
inflate :: (Num a, Ord a) => a -> Interval a -> Interval a
inflate x y = symmetric x + y

-- | Deflate an interval by shrinking it from both ends.
--
-- >>> deflate 3 (-4 ... -10)
-- -1 ... -7
--
-- >>> deflate 2 (-1 ... 1)
-- 1 ... -1
deflate :: Num a => a -> Interval a -> Interval a
deflate x (I a b) = I a' b'
  where
    a' = a + x
    b' = b - x

-- | Construct a symmetric interval.
--
-- >>> symmetric 3
-- -3 ... 3
--
-- >>> symmetric (-2)
-- 2 ... -2
symmetric :: Num a => a -> Interval a
symmetric x = negate x ... x

instance (Num a, Ord a) => Num (Interval a) where
  I a b + I a' b' = (a + a') ... (b + b')
  {-# INLINE (+) #-}
  I a b - I a' b' = (a - b') ... (b - a')
  {-# INLINE (-) #-}
  I a b * I a' b' =
    minimum [a * a', a * b', b * a', b * b']
    ...
    maximum [a * a', a * b', b * a', b * b']
  {-# INLINE (*) #-}
  abs x @(I a b)
    | a >= 0         = x
    | b <= 0         = negate x
    | b > 0 && a < 0 = 0 ... max (-a) b
    | otherwise      = x
  {-# INLINE abs #-}

  signum = increasing signum
  {-# INLINE signum #-}

  fromInteger i = singleton (fromInteger i)
  {-# INLINE fromInteger #-}

bisectIntegral :: Integral a => Interval a -> (Interval a, Interval a)
bisectIntegral (I a b)
  | a == m || b == m = (I a a, I b b)
  | otherwise        = (I a m, I m b)
  where m = a + (b - a) `div` 2
{-# INLINE bisectIntegral #-}

-- | Determine if a point is in the interval.
--
-- >>> member 3 (1 ... 5)
-- True
--
-- >>> member 5 (1 ... 5)
-- True
--
-- >>> member 1 (1 ... 5)
-- True
--
-- >>> member 8 (1 ... 5)
-- False
member :: Ord a => a -> Interval a -> Bool
member x (I a b) = x >= a && x <= b
{-# INLINE member #-}

-- | Determine if a point is not in the interval.
--
-- >>> notMember 8 (1 ... 5)
-- True
--
-- >>> notMember 2 (1 ... 5)
-- False
notMember :: Ord a => a -> Interval a -> Bool
notMember x xs = not (member x xs)
{-# INLINE notMember #-}

-- | lift a monotone increasing function over a given interval
increasing :: (a -> b) -> Interval a -> Interval b
increasing f (I a b) = f a ... f b

-- | lift a monotone decreasing function over a given interval
decreasing :: (a -> b) -> Interval a -> Interval b
decreasing f (I a b) = f b ... f a

-- | Attempt to merge two intervals.
--
-- If the two intervals are overlapping or adjacent, merge the
-- two intervals. Otherwise, 'Nothing' is returned.
merge :: (Enum a, Ord a) => Interval a -> Interval a -> Maybe (Interval a)
merge a@(I x y) b@(I u v) =
  I (min x u) (max y v) <$ guard (a ==? b || adjacent a b)

-- | Calculate the convex hull of two intervals
--
-- >>> hull (0 ... 10 :: Interval Int) (5 ... 15 :: Interval Int)
-- 0 ... 15
--
-- >>> hull (15 ... 85 :: Interval Int) (0 ... 10 :: Interval Int)
-- 0 ... 85
hull :: Ord a => Interval a -> Interval a -> Interval a
hull x@(I a b) y@(I a' b')
  | not (valid x) = y
  | not (valid y) = x
  | otherwise = min a a' ... max b b'
{-# INLINE hull #-}

-- | Check if interval @X@ totally contains interval @Y@
--
-- >>> (20 ... 40 :: Interval Int) `contains` (25 ... 35 :: Interval Int)
-- True
--
-- >>> (20 ... 40 :: Interval Int) `contains` (15 ... 35 :: Interval Int)
-- False
contains :: Ord a => Interval a -> Interval a -> Bool
contains x y = not (valid y)
            || (valid x && inf x <= inf y && sup y <= sup x)
{-# INLINE contains #-}

-- | Flipped version of `contains`. Check if interval @X@ a subset of interval @Y@
--
-- >>> (25 ... 35 :: Interval Int) `isSubsetOf` (20 ... 40 :: Interval Int)
-- True
--
-- >>> (20 ... 40 :: Interval Int) `isSubsetOf` (15 ... 35 :: Interval Int)
-- False
isSubsetOf :: Ord a => Interval a -> Interval a -> Bool
isSubsetOf = flip contains
{-# INLINE isSubsetOf #-}

-- | Are the two intervals adjacent?
--
-- >>> (1 ... 5 :: Interval Int) `adjacent` (5 ... 10 :: Interval Int)
-- False
--
-- >>> (1 ... 6 :: Interval Int) `adjacent` (7 ... 10 :: Interval Int)
-- True
adjacent :: (Enum a, Ord a) => Interval a -> Interval a -> Bool
adjacent x y = succ (sup x) == inf y || succ (sup y) == inf x

-- | For all @x@ in @X@, @y@ in @Y@. @x '<' y@
--
-- >>> (5 ... 10 :: Interval Int) <! (20 ... 30 :: Interval Int)
-- True
--
-- >>> (5 ... 10 :: Interval Int) <! (10 ... 30 :: Interval Int)
-- False
--
-- >>> (20 ... 30 :: Interval Int) <! (5 ... 10 :: Interval Int)
-- False
(<!)  :: Ord a => Interval a -> Interval a -> Bool
x <! y = sup x < inf y
{-# INLINE (<!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '<=' y@
--
-- >>> (5 ... 10 :: Interval Int) <=! (20 ... 30 :: Interval Int)
-- True
--
-- >>> (5 ... 10 :: Interval Int) <=! (10 ... 30 :: Interval Int)
-- True
--
-- >>> (20 ... 30 :: Interval Int) <=! (5 ... 10 :: Interval Int)
-- False
(<=!) :: Ord a => Interval a -> Interval a -> Bool
x <=! y = sup x <= inf y
{-# INLINE (<=!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '==' y@
--
-- Only singleton intervals return true
--
-- >>> (singleton 5 :: Interval Int) ==! (singleton 5 :: Interval Int)
-- True
--
-- >>> (5 ... 10 :: Interval Int) ==! (5 ... 10 :: Interval Int)
-- False
(==!) :: Eq a => Interval a -> Interval a -> Bool
x ==! y = sup x == inf y && inf x == sup y
{-# INLINE (==!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '/=' y@
--
-- >>> (5 ... 15 :: Interval Int) /=! (20 ... 40 :: Interval Int)
-- True
--
-- >>> (5 ... 15 :: Interval Int) /=! (15 ... 40 :: Interval Int)
-- False
(/=!) :: Ord a => Interval a -> Interval a -> Bool
x /=! y = sup x < inf y || inf x > sup y
{-# INLINE (/=!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '>' y@
--
-- >>> (20 ... 40 :: Interval Int) >! (10 ... 19 :: Interval Int)
-- True
--
-- >>> (5 ... 20 :: Interval Int) >! (15 ... 40 :: Interval Int)
-- False
(>!)  :: Ord a => Interval a -> Interval a -> Bool
x >! y = inf x > sup y
{-# INLINE (>!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '>=' y@
--
-- >>> (20 ... 40 :: Interval Int) >=! (10 ... 20 :: Interval Int)
-- True
--
-- >>> (5 ... 20 :: Interval Int) >=! (15 ... 40 :: Interval Int)
-- False
(>=!) :: Ord a => Interval a -> Interval a -> Bool
x >=! y = inf x >= sup y
{-# INLINE (>=!) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '<' y@?
(<?) :: Ord a => Interval a -> Interval a -> Bool
x <? y = inf x < sup y
{-# INLINE (<?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '<=' y@?
(<=?) :: Ord a => Interval a -> Interval a -> Bool
x <=? y = inf x <= sup y
{-# INLINE (<=?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '==' y@?
(==?) :: Ord a => Interval a -> Interval a -> Bool
x ==? y = inf x <= sup y && sup x >= inf y
{-# INLINE (==?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '/=' y@?
(/=?) :: Eq a => Interval a -> Interval a -> Bool
x /=? y = inf x /= sup y || sup x /= inf y
{-# INLINE (/=?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '>' y@?
(>?) :: Ord a => Interval a -> Interval a -> Bool
x >? y = sup x > inf y
{-# INLINE (>?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '>=' y@?
(>=?) :: Ord a => Interval a -> Interval a -> Bool
x >=? y = sup x >= inf y
{-# INLINE (>=?) #-}

