
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






newtype PartialP a b = PartialP { getPartialP :: a -> Maybe (a, b) }
    
instance Functor (PartialP r) where
    fmap f (PartialP g) = PartialP (fmap (fmap f) . g)

instance Monad (PartialP r) where
    return x = PartialP (\a -> Just (a, x))
    PartialP f >>= k = PartialP $ \r -> (f r >>= \(r1, x) -> getPartialP (k x) r1)

instance MonadPlus (PartialP r) where
    mzero = PartialP (const Nothing)
    PartialP f `mplus` PartialP g = PartialP $ \x -> f x `mplus` g x

instance Applicative (PartialP r) where
    pure  = return
    (<*>) = ap

instance Alternative (PartialP r) where
    empty = mzero
    (<|>) = mplus

instance Semigroup (PartialP a b) where
    (<>) = mplus

instance Monoid (PartialP a b) where
    mempty  = mzero
    mappend = mplus




newtype SparseT a b = SparseT { getSparseT :: PartialP a b }
    deriving (Semigroup, Monoid, Functor, Applicative, Alternative, Monad, MonadPlus)

type Sparse = SparseT String

runSparseT :: SparseT a b -> a -> Maybe b
runSparseT = fmap (fmap snd) . getPartialP . getSparseT

runSparseT' :: SparseT a b -> a -> Maybe (a, b)
runSparseT' = getPartialP . getSparseT

runSparse :: Sparse a -> String -> Maybe a
runSparse = runSparseT

headP :: (a -> Bool) -> [a] -> Maybe ([a], a)
headP p []     = Nothing
headP p (x:xs) = if not (p x) then Nothing else Just (xs, x)

splitN :: ([a] -> Int) -> [a] -> Maybe ([a], [a])
splitN p [] = Nothing
splitN p ys = let n = p ys in if n < 1 then Nothing else Just (drop n ys, take n ys)


char :: Char -> Sparse Char
char c = SparseT $ PartialP $ headP (== c)







single x = [x]
list z f xs = case xs of
    [] -> z
    ys -> f ys

[a,b,c,d,e,f,g,x,y,z,m,n,o,p,q,r] = undefined