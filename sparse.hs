
{-# LANGUAGE GeneralizedNewtypeDeriving,
    TypeOperators,
    DeriveFunctor,
    DeriveFoldable,
    TypeSynonymInstances
    #-}


module Sparse where

import Data.Semigroup
import Data.Foldable(Foldable)
import Control.Applicative
import Control.Monad.Plus

-- TODO
instance Semigroup (Partial a b) where (<>) = mplus






newtype a ?-> b = PartialP { getPartialP :: a -> Maybe (a, b) }
    
instance Functor ((?->) r) where
    fmap f (PartialP g) = PartialP (fmap (fmap f) . g)

instance Monad ((?->) r) where
    return x = PartialP (\a -> Just (a, x))
    PartialP f >>= k = PartialP $ \r -> (f r >>= \(r1, x) -> getPartialP (k x) r1)

instance MonadPlus ((?->) r) where
    mzero = PartialP (const Nothing)
    PartialP f `mplus` PartialP g = PartialP $ \x -> f x `mplus` g x

instance Applicative ((?->) r) where
    pure  = return
    (<*>) = ap

instance Alternative ((?->) r) where
    empty = mzero
    (<|>) = mplus

instance Semigroup ((?->) a b) where
    (<>) = mplus

instance Monoid ((?->) a b) where
    mempty  = mzero
    mappend = mplus




-- newtype SparseT a b = SparseT { getSparseT :: a ?-> b }
type SparseT = (?->)
    -- deriving (Semigroup, Monoid, Functor, Applicative, Alternative, Monad, MonadPlus)

type Sparse = SparseT String

runSparseT :: SparseT a b -> a -> Maybe b
runSparseT = fmap (fmap snd) . getPartialP

runSparseT' :: SparseT a b -> a -> Maybe (a, b)
runSparseT' = getPartialP

runSparse :: Sparse a -> String -> Maybe a
runSparse = runSparseT

----------


headP :: (a -> Bool) -> [a] -> Maybe ([a], a)
headP p []     = Nothing
headP p (x:xs) = if not (p x) then Nothing else Just (xs, x)

splitN :: ([a] -> Int) -> [a] -> Maybe ([a], [a])
splitN p [] = Nothing
splitN p ys = let n = p ys in if n < 1 then Nothing else Just (drop n ys, take n ys)

----------

char :: Char -> Sparse Char
char c = charIs (== c)

charIs :: (Char -> Bool) -> Sparse Char
charIs p = PartialP $ headP p

string :: String -> Sparse String
string s = stringIs (length s) (== s)

stringIs :: Int -> (String -> Bool) -> Sparse String
stringIs n p = PartialP $ splitN (\xs -> if p (take n xs) then n else 0)
----------




single x = [x]
list z f xs = case xs of
    [] -> z
    ys -> f ys

[a,b,c,d,e,f,g,x,y,z,m,n,o,p,q,r] = undefined