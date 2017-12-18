{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Diet.Map where

import           Data.Bits     (shiftR, (.&.))
import           Data.Bool     (bool)
import qualified Data.Foldable as F
import           Data.Functor.Classes
import           Data.Interval (Interval(..))
import qualified Data.Interval as I
import qualified Data.List     as L
import qualified Data.Map.Strict as M
import qualified GHC.Exts      as GHCExts

import Prelude hiding (lookup, map, null, foldr, foldr', foldl, foldl')

data Color = R | B
  deriving (Eq, Read, Show)

data Map k v = BR !Color !(Interval k) !(Interval k) v !(Map k v) !(Map k v)
             | LF

instance (Enum k, Eq k, Ord k, Eq v) => Eq (Map k v) where
  m1 == m2 = toAscList m1 == toAscList m2

instance (Enum k, Ord k, Ord v) => Ord (Map k v) where
  compare m1 m2 = compare (toAscList m1) (toAscList m2)

instance (Enum k, Ord k, Eq v, Monoid v) => Monoid (Map k v) where
  mempty  = empty
  mappend = unionAppend
  mconcat = unions

instance (Enum k, Ord k) => Functor (Map k) where
  fmap = map

instance (Enum k, Ord k) => F.Foldable (Map k) where
  fold LF = mempty
  fold (BR _ _ _ v l r) = F.fold l `mappend` v `mappend` F.fold r
  foldr  = foldr
  foldr' = foldr'
  foldl  = foldl
  foldl' = foldl' 
  foldMap _ LF = mempty
  foldMap f (BR _ _ _ v l r) = F.foldMap f l `mappend` f v `mappend` F.foldMap f r
  null   = null
  length = size

instance (Enum k, Ord k) => Traversable (Map k) where
  traverse _ LF = pure LF
  traverse f (BR c k m v l r)
    = flip (BR c k m) <$> traverse f l <*> f v <*> traverse f r

instance (Enum k, Ord k) => GHCExts.IsList (Map k v) where
  type Item (Map k v) = (Interval k, v)
  fromList = fromList
  toList   = toList

instance (Enum k, Ord k, Show k, Show v) => Show (Map k v) where
  showsPrec n d = showParen (n > 10) $ showString "fromList " . shows (toList d)

empty :: Map k v
empty = LF
{-# INLINE empty #-}

singleton :: Interval k -> v -> Map k v
singleton k v = BR B k k v LF LF
{-# INLINE singleton #-}

mNode :: (Enum k, Ord k) => Color -> Interval k -> v -> Map k v -> Map k v -> Map k v
mNode c k v l r = BR c k (maxUpper k l r) v l r
  where
    maxUpper :: (Enum k, Ord k) => Interval k -> Map k v -> Map k v -> Interval k
    maxUpper k LF LF = k
    maxUpper k LF (BR _ _ m _ _ _) = potentialBeyonceBetrayal k m
    maxUpper k (BR _ _ m _ _ _) LF = potentialBeyonceBetrayal k m
    maxUpper k (BR _ _ l _ _ _) (BR _ _ r _ _ _) = potentialBeyonceBetrayal k (potentialBeyonceBetrayal l r)
    
    potentialBeyonceBetrayal :: (Enum k, Ord k) => Interval k -> Interval k -> Interval k
    potentialBeyonceBetrayal = (\x y -> if I.sup x >= I.sup y then x else y)

null :: Map k v -> Bool
null LF = True
null _  = False
{-# INLINE null #-}

size :: Map k v -> Int
size m = go 0 m
  where
    go !p q
      = case q of
        LF -> p
        BR _ _ _ _ l r -> go (go p l + 1) r
{-# INLINE size #-}

isRed :: Map k v -> Bool
isRed (BR R _ _ _ _ _) = True
isRed _ = False

isBlack :: Map k v -> Bool
isBlack = not . isRed

turnBlack :: Map k v -> Map k v
turnBlack (BR R k m vs l r) = BR B k m vs l r
turnBlack t = t

turnRed :: Map k v -> Map k v
turnRed (BR B k m vs l r) = BR R k m vs l r
turnRed t = t

lookup :: (Enum k, Ord k) => Interval k -> Map k v -> Maybe v
lookup !k LF = Nothing
lookup !k (BR _ key _ v l r)
  = case compare k key of
    LT -> lookup k l
    GT -> lookup k r
    EQ -> Just v

insertAppend :: (Enum k, Ord k, Monoid v) => Interval k -> v -> Map k v -> Map k v
insertAppend = insertWithKey (\_ x y -> x `mappend` y)

insert :: (Enum k, Ord k) => Interval k -> v -> Map k v -> Map k v
insert = insertWithKey (\_ v _ -> v)

insertWith :: (Enum k, Ord k) => (v -> v -> v) -> Interval k -> v -> Map k v -> Map k v
insertWith f = insertWithKey (\_ new old -> f new old)

insertWithKey :: (Enum k, Ord k) => (Interval k -> v -> v -> v) -> Interval k -> v -> Map k v -> Map k v
insertWithKey f !key value mp = bool mp (turnBlack (ins mp)) (I.valid key)
  where
    singletonRed k v = BR R k k v LF LF 
    ins LF = singletonRed key value
    ins (BR color k m v l r) =
      case compare key k of
        LT -> balanceL color k v (ins l) r
        GT -> balanceR color k v l (ins r)
        EQ -> BR color k m (f k value v) l r

unionAppend :: (Enum k, Ord k, Eq v, Monoid v) => Map k v -> Map k v -> Map k v
unionAppend = (\m m' -> foldlWithKey' ((\f x y z -> f y z x) insertAppend) m m')

union :: (Enum k, Ord k) => Map k v -> Map k v -> Map k v
union m1 m2 = unionWithKey (\_ v _ -> v) m1 m2

unionWith :: (Enum k, Ord k) => (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith f m1 m2 = unionWithKey (\_ v1 v2 -> f v1 v2) m1 m2

unionWithKey :: (Enum k, Ord k) => (Interval k -> v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWithKey f m1 m2 = fromDistinctAscList (ascListUnion f (toAscList m1) (toAscList m2))

unionsAppend :: (Enum k, Ord k, Monoid v) => [Map k v] -> Map k v
unionsAppend = unionsWith mappend

unions :: (Enum k, Ord k) => [Map k v] -> Map k v
unions = unionsWith const

unionsWith :: (Enum k, Ord k) => (v -> v -> v) -> [Map k v] -> Map k v
unionsWith _ [] = empty
unionsWith _ [m] = m
unionsWith f ms = fromDistinctAscList (head (go (L.map toAscList ms)))
  where
    go [] = []
    go xs@[_] = xs
    go (x:y:xs) = go ((\m1 m2 -> ascListUnion (\x y z -> f y z) m1 m2) x y : go xs)

foldr :: (Enum k, Ord k) => (a -> b -> b) -> b -> Map k a -> b
foldr _ z LF = z
foldr f z (BR _ _ _ x l r) = foldr f (f x (foldr f z r)) l

foldr' :: (Enum k, Ord k) => (a -> b -> b) -> b -> Map k a -> b
foldr' _ !z LF = z
foldr' f !z (BR _ _ _ x l r) = foldr' f (f x (foldr' f z r)) l

foldl :: (Enum k, Ord k) => (b -> a -> b) -> b -> Map k a -> b
foldl _ z LF = z
foldl f z (BR _ _ _ x l r) = foldl f (f (foldl f z l) x) r

foldl' :: (Enum k, Ord k) => (b -> a -> b) -> b -> Map k a -> b
foldl' _ !z LF = z
foldl' f !z (BR _ _ _ x l r) = foldl' f (f (foldl' f z l) x) r

foldrWithKey :: (Enum k, Ord k) => (Interval k -> v -> a -> a) -> a -> Map k v -> a
foldrWithKey _ z LF = z
foldrWithKey f z (BR _ k _ x l r) = foldrWithKey f (f k x (foldrWithKey f z r)) l

foldrWithKey' :: (Enum k, Ord k) => (Interval k -> v -> a -> a) -> a -> Map k v -> a
foldrWithKey' _ !z LF = z
foldrWithKey' f !z (BR _ k _ x l r) = foldrWithKey' f (f k x (foldrWithKey' f z r)) l

foldlWithKey :: (Enum k, Ord k) => (a -> Interval k -> v -> a) -> a -> Map k v -> a
foldlWithKey _ z LF = z
foldlWithKey f z (BR _ k _ x l r) = foldlWithKey f (f (foldlWithKey f z l) k x) r

foldlWithKey' :: (Enum k, Ord k) => (a -> Interval k -> v -> a) -> a -> Map k v -> a
foldlWithKey' _ !z LF = z
foldlWithKey' f !z (BR _ k _ x l r) = foldlWithKey' f (f (foldlWithKey' f z l) k x) r

map :: (Enum k, Ord k) => (a -> b) -> Map k a -> Map k b
map f = mapWithKey (\_ x -> f x)

mapWithKey :: (Enum k, Ord k) => (Interval k -> a -> b) -> Map k a -> Map k b
mapWithKey f = go
  where
    go LF = LF
    go (BR c k m v l r) = BR c k m (f k v) (go l) (go r)

toAscList :: (Enum k, Ord k) => Map k v -> [(Interval k,v)]
toAscList = foldrWithKey (\k v r -> (k,v) : r) []

toDescList :: (Enum k, Ord k) => Map k v -> [(Interval k,v)]
toDescList = foldlWithKey (\r k v -> (k,v) : r) []

toList :: (Enum k, Ord k) => Map k v -> [(Interval k, v)]
toList = toAscList

fromList :: (Enum k, Ord k) => [(Interval k,v)] -> Map k v
fromList = L.foldl' (\m (k,v) -> insert k v m) empty 

ascListUnion :: (Enum k, Ord k) => (Interval k -> a -> a -> a) -> [(Interval k, a)] -> [(Interval k, a)] -> [(Interval k, a)]
ascListUnion _ [] [] = []
ascListUnion _ [] ys = ys
ascListUnion _ xs [] = xs
ascListUnion f xs@(x@(xk,xv):xs') ys@(y@(yk,yv):ys')
  = case compare xk yk of
    LT -> x : ascListUnion f xs' ys
    GT -> y : ascListUnion f xs ys'
    EQ -> (xk, f xk xv yv) : ascListUnion f xs' ys'

-- strict tuple
data T2 a b = T2 !a !b

fromDistinctAscList :: (Enum k, Ord k) => [(Interval k,v)] -> Map k v
fromDistinctAscList ls
  = case go (length ls) ls of
      (T2 result []) -> result
      _ -> error "Data.Diet.Map.fromDistinctAscList: list not fully consumed"
  where
    go n xs 
      | n == 0      = T2 LF xs
      | isPerfect n = buildB n xs
      | otherwise   = buildR n (log2 n) xs
    buildB n !xs
      | n <= 0 = error "Data.Diet.Map.fromDistinctAscList: buildB 0"
      | n == 1
          = case xs of 
              ((k,v):xs') -> T2 (BR B k k v LF LF) xs'
              _           -> error "Data.Diet.Map.fromDistinctAscList: buildB 1"
      | otherwise
        = case n `quot` 2 of
            -- look ma, i'm writing java now! 
            { n' -> case buildB n' xs of
              { (T2 _ []) -> error "Data.Diet.Map.fromDistinctAscList: buildB n";
                (T2 l ((k,v):xs')) -> case buildB n' xs' of
                  { (T2 r xs'') -> T2 (mNode B k v l r) xs'' }
              }
            }
    buildR n !d !xs
      | n == 0 = T2 LF xs
      | n == 1
        = case xs of
            ((k,v):xs') -> T2 (BR (if d == 0 then R else B) k k v LF LF) xs'
            _           -> error "Data.Diet.Map.fromDistinctAscList: buildR 1"
      | otherwise
        = case n `quot` 2 of
            -- ma, no hands!
            { n' -> case buildR n' (d - 1) xs of
              { (T2 _ []) -> error "Data.Diet.Map.fromDistinctAscList: buildR n";
                (T2 l ((k,v):xs')) -> case buildR (n - (n' + 1)) (d - 1) xs' of
                  { (T2 r xs'') -> T2 (mNode B k v l r) xs''}
              }
            }

-- a Tree t is perfect if its size n == 2^m - 1 for any m <- N
isPerfect :: Int -> Bool
isPerfect n = (n .&. (n + 1)) == 0

log2 :: Int -> Int
log2 m = go (-1) m
  where
    go !p q
      | q <= 0 = p
      | otherwise = go (p + 1) (q `shiftR` 1)

data Shrunk k v = U !(Map k v)
                | S !(Map k v)

unwrap :: Shrunk k v -> Map k v
unwrap (U m) = m
unwrap (S m) = m

data ShrunkVal k v a = U' !(Map k v) a
                     | S' !(Map k v) a

unwrap' :: ShrunkVal k v a -> Map k v
unwrap' (U' m _) = m
unwrap' (S' m _) = m

annotate :: Shrunk k v -> a -> ShrunkVal k v a
annotate (U m) x = U' m x
annotate (S m) x = S' m x

delete :: (Enum k, Ord k) => Interval k -> Map k v -> Map k v
delete key mp = turnBlack (unwrap (delete' key mp))

delete' :: (Enum k, Ord k) => Interval k -> Map k v -> Shrunk k v
delete' !x LF = U LF
delete' !x (BR c k _ v l r)
  = case compare x k of
      LT -> case delete' x l of
        (U l') -> U (mNode c k v l' r)
        (S l') -> unbalancedR c k v l' r
      GT -> case delete' x r of
        (U r') -> U (mNode c k v l r')
        (S r') -> unbalancedL c k v l r'
      EQ -> case r of
              LF -> if c == B then blackify l else U l
              _  -> case deleteMin' r of
                (U' r' (rk,rv)) -> U (mNode c rk rv l r')
                (S' r' (rk,rv)) -> unbalancedL c rk rv l r'

deleteMin :: (Enum k, Ord k) => Map k v -> Map k v
deleteMin LF = LF
deleteMin x = turnBlack (unwrap' (deleteMin' x))

deleteMin' :: (Enum k, Ord k) => Map k v -> ShrunkVal k v (Interval k,v)
deleteMin' LF = error "Data.Diet.Map.deleteMin': no min in a leaf node" 
deleteMin' (BR B k _ v LF LF) = S' LF (k,v)
deleteMin' (BR B k _ v LF r@(BR R _ _ _ _ _)) = U' (turnBlack r) (k,v)
deleteMin' (BR R k _ v LF r) = U' r (k,v)
deleteMin' (BR c k _ v l r)
  = case deleteMin' l of
    (U' l' kv) -> U' (mNode c k v l' r) kv
    (S' l' kv) -> annotate (unbalancedR c k v l' r) kv

blackify :: Map k v -> Shrunk k v
blackify (BR R k m v l r) = U (BR B k m v l r)
blackify t = S t

balanceL :: (Enum k, Ord k) => Color -> Interval k -> v -> Map k v -> Map k v -> Map k v
balanceL B zk zv (BR R yk _ yv (BR R xk _ xv a b) c) d
  = mNode R yk yv (mNode B xk xv a b) (mNode B zk zv c d)
balanceL B zk zv (BR R xk _ xv a (BR R yk _ yv b c)) d
  = mNode R yk yv (mNode B xk xv a b) (mNode B zk zv c d)
balanceL c xk xv l r = mNode c xk xv l r

balanceR :: (Enum k, Ord k) => Color -> Interval k -> v -> Map k v -> Map k v -> Map k v
balanceR B xk xv a (BR R yk _ yv b (BR R zk _ zv c d)) 
  = mNode R yk yv (mNode B xk xv a b) (mNode B zk zv c d)
balanceR B xk xv a (BR R zk _ zv (BR R yk _ yv b c) d)
  = mNode R yk yv (mNode B xk xv a b) (mNode B zk zv c d)
balanceR c xk xv l r = mNode c xk xv l r

unbalancedL :: (Enum k, Ord k) => Color -> Interval k -> v -> Map k v -> Map k v -> Shrunk k v
unbalancedL R k v l@(BR B _ _ _ _ _) r = U (balanceL B k v (turnRed l) r)
unbalancedL B k v l@(BR B _ _ _ _ _) r = S (balanceL B k v (turnRed l) r)
unbalancedL B k v (BR R lk _ lv ll lr@(BR B _ _ _ _ _)) r
  = U (mNode B lk lv ll (balanceL B k v (turnRed lr) r))
unbalancedL _ _ _ _ x = U x

unbalancedR :: (Enum k, Ord k) => Color -> Interval k -> v -> Map k v -> Map k v -> Shrunk k v
unbalancedR B k v l r@(BR B _ _ _ _ _) = S (balanceR B k v l (turnRed r))
unbalancedR R k v l r@(BR B _ _ _ _ _) = U (balanceR B k v l (turnRed r))
unbalancedR B k v l (BR R rk _ rv rl@(BR B _ _ _ _ _) rr)
  = U (mNode B rk rv (balanceR B k v l (turnRed rl)) rr)
unbalancedR _ _ _ _ x = U x


