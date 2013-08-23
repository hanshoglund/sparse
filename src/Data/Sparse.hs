
{-# LANGUAGE GeneralizedNewtypeDeriving,
    OverloadedStrings,
    TypeOperators,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleInstances
    #-}


module Data.Sparse (
        -- * Sparse
        SparseT,
        Sparse,
        asSparse,

        -- * Running
        runSparseT,
        runSparseT',
        runSparse,
        runSparse',

        -- * Primitives
        headP,
        splitP,
        
        -- * Basic parsers
        char,
        charIs,
        string,
        stringIs,

        -- * Combinators
        optionally,
        optionallyMaybe,
        Data.Sparse.optional,
        between,
        skipMany1,
        skipMany,
        many1,
        sepBy,
        sepBy1,
        sepEndBy1,
        sepEndBy,
        endBy1,
        endBy,
        count
) where

import Data.String
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


----------

newtype SparseT a b = SparseT { getSparseT :: a ?-> b }
    deriving (Semigroup, Monoid, Functor, Applicative, Alternative, Monad, MonadPlus)

instance IsString (SparseT String String) where
    fromString = string

type Sparse = SparseT String

runSparseT :: SparseT a b -> a -> Maybe b
runSparseT = fmap (fmap snd) . runSparseT'

runSparseT' :: SparseT a b -> a -> Maybe (a, b)
runSparseT' = getPartialP . getSparseT

runSparse :: Sparse a -> String -> Maybe a
runSparse = runSparseT

runSparse' :: Sparse a -> String -> Maybe (String, a)
runSparse' = runSparseT'

----------

headP  = SparseT . PartialP . headP'
splitP = SparseT . PartialP . splitP'

headP' :: (a -> Bool) -> [a] -> Maybe ([a], a)
headP' p []     = Nothing
headP' p (x:xs) = if not (p x) then Nothing else Just (xs, x)

splitP' :: ([a] -> Int) -> [a] -> Maybe ([a], [a])
splitP' p [] = Nothing
splitP' p ys = let n = p ys in if n < 1 then Nothing else Just (drop n ys, take n ys)

----------

char :: Char -> Sparse Char
char c = charIs (== c)

charIs :: (Char -> Bool) -> Sparse Char
charIs p = headP p

string :: String -> Sparse String
string s = stringIs (length s) (== s)

stringIs :: Int -> (String -> Bool) -> Sparse String
stringIs n p = splitP (\xs -> if p (take n xs) then n else 0)

asSparse = id
asSparse :: Sparse a -> Sparse a

----------

optionally x p          = p <|> return x
optionallyMaybe p       = optionally Nothing (liftM Just p)
optional p          = do{ p; return ()} <|> return ()
between open close p
                    = do{ open; x <- p; close; return x }
skipMany1 p         = do{ p; skipMany p }
skipMany p          = scan
                    where
                      scan  = do{ p; scan } <|> return ()
many1 p             = do{ x <- p; xs <- many p; return (x:xs) }
sepBy p sep         = sepBy1 p sep <|> return []
sepBy1 p sep        = do{ x <- p
                        ; xs <- many (sep >> p)
                        ; return (x:xs)
                        }
sepEndBy1 p sep     = do{ x <- p
                        ; do{ sep
                            ; xs <- sepEndBy p sep
                            ; return (x:xs)
                            }
                          <|> return [x]
                        }
sepEndBy p sep      = sepEndBy1 p sep <|> return []
endBy1 p sep        = many1 (do{ x <- p; sep; return x })
endBy p sep         = many (do{ x <- p; sep; return x })
count n p           | n <= 0    = return []
                    | otherwise = sequence (replicate n p)

----------


-- test :: Sparse [String]
test = asSparse $ string "hans" >> many1 (string ";")




single x = [x]
list z f xs = case xs of
    [] -> z
    ys -> f ys

[a,b,c,d,e,f,g,x,y,z,m,n,o,p,q,r] = undefined