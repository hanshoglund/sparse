
{-# LANGUAGE GeneralizedNewtypeDeriving,
    OverloadedStrings,
    TypeOperators,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleInstances
    #-}


-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (GNTD, DeriveFunctor, OverloadedStrings)
--
-- Lightweight parsing library based on partial functions.
--
-------------------------------------------------------------------------------------

module Data.Sparse (
        -- * Sparse
        SparseT,
        Sparse,
        asSparse,

        -- * Running
        runSparseT,
        runSparseT',
        runSparse,
        withState,

        -- * Primitives
        stateP,
        mapStateP,
        mapInputP,
        headP,
        splitP,

        -- * Basic parsers
        char,
        charIf,
        string,
        stringIf,

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

import Data.Char
import Data.String
import Data.Pointed
import Data.Semigroup
import Data.Foldable(Foldable)
import Control.Applicative
import Control.Monad.Plus


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

instance Pointed ((?->) r) where
    point = return

instance Semigroup ((?->) a b) where
    (<>) = mplus

instance Monoid ((?->) a b) where
    mempty  = mzero
    mappend = mplus


----------

newtype SparseT s a b = SparseT { getSparseT :: (s, [a]) ?-> b }
    deriving (Semigroup, Monoid, Functor, Pointed, Applicative, Alternative, Monad, MonadPlus)

instance IsString (SparseT a Char String) where
    fromString = string

type Sparse = SparseT () Char

runSparseT :: SparseT s a b -> s -> [a] -> Maybe b
runSparseT = curry . fmap (fmap snd) . getPartialP . getSparseT

runSparseT' :: SparseT s a b -> s -> [a] -> Maybe ((s, [a]), b)
runSparseT' = curry . getPartialP . getSparseT

runSparse :: Sparse a -> String -> Maybe a
runSparse p = runSparseT p ()

runSparse' :: Sparse a -> String -> Maybe (((), String), a)
runSparse' p = runSparseT' p ()

withState :: (s -> t) -> (t -> s) -> SparseT t a b -> SparseT s a b
withState setup teardown (SparseT (PartialP f)) = (SparseT (PartialP $ ws f))
    where
        ws f = fmap (first (first teardown)) . f . first setup


----------

-- | Return the state as result.
stateP :: SparseT s a s
stateP = (SparseT (PartialP st))
    where
        st = \(s, as) -> Just ((s, as), s)

-- | Transform state.
mapStateP :: (s -> s) -> SparseT s a ()
mapStateP f = (SparseT (PartialP st))
    where
        st = \(s, as) -> Just ((f s, as), ())

-- | Transform input.
mapInputP :: ([a] -> [a]) -> SparseT s a ()
mapInputP f = (SparseT (PartialP st))
    where
        st = \(s, as) -> Just ((s, f as), ())


-- | Consumes one input element.
--
--   Fails if the predicate fails, or if there is no more input.
--
headP :: (s -> a -> Bool) -> SparseT s a a
headP  = SparseT . PartialP . headP'

-- | Consume one or more input elements.
--
--   The given function receives the /entire/ remaining input, and must return
--   the number of consumed elements.
--
--   Fails if the predicate return 0 or less, or if there is no more input.
--
splitP :: (s -> [a] -> Int) -> SparseT s a [a]
splitP = SparseT . PartialP . splitP'

headP' :: (s -> a -> Bool) -> (s, [a]) -> Maybe ((s, [a]), a)
headP' p (s, [])     = Nothing
headP' p (s, (x:xs)) = if not (p s x) then Nothing else Just ((s, xs), x)

splitP' :: (s -> [a] -> Int) -> (s, [a]) -> Maybe ((s, [a]), [a])
splitP' p (s, []) = Nothing
splitP' p (s, ys) = let n = p s ys in if n < 1 then Nothing else Just ((s, drop n ys), take n ys)


----------

-- char :: Char -> Sparse Char
char c = charIf (== c)

-- charIf :: (Char -> Bool) -> Sparse Char
charIf p = headP (const p)

-- string :: String -> Sparse String
string s = stringIf (length s) (== s)

-- stringIf :: Int -> (String -> Bool) -> Sparse String
stringIf n p = splitP (\_ xs -> if p (take n xs) then n else 0)

asSparse = id
asSparse :: Sparse a -> Sparse a

----------

optionally x p          = p <|> return x
optionallyMaybe p       = optionally Nothing (liftM Just p)
optional p              = do{ p; return ()} <|> return ()
between open close p
                        = do{ open; x <- p; close; return x }
skipMany1 p             = do{ p; skipMany p }
skipMany p              = scan
                        where
                          scan  = do{ p; scan } <|> return ()
many1 p                 = do{ x <- p; xs <- many p; return (x:xs) }
sepBy p sep             = sepBy1 p sep <|> return []
sepBy1 p sep            = do{ x <- p
                            ; xs <- many (sep >> p)
                            ; return (x:xs)
                            }
sepEndBy1 p sep         = do{ x <- p
                            ; do{ sep
                                ; xs <- sepEndBy p sep
                                ; return (x:xs)
                                }
                              <|> return [x]
                            }
sepEndBy p sep          = sepEndBy1 p sep <|> return []
endBy1 p sep            = many1 (do{ x <- p; sep; return x })
endBy p sep             = many (do{ x <- p; sep; return x })
count n p               | n <= 0    = return []
                        | otherwise = sequence (replicate n p)

----------

space  = many (charIf isSpace)
symbol = many (charIf isAlphaNum)

----------


test :: SparseT Int Char String
test = withState id id $ do
    string "name:"
    space        
    n <- symbol
    m <- withState (+ 10) (subtract 10) stateP
    space
    many1 (string ";")
    space
    return ("Name is " ++ n ++ ", state is " ++ show m)




first f (a, b) = (f a, b)
single x = [x]
list z f xs = case xs of
    [] -> z
    ys -> f ys

[a,b,c,d,e,f,g,x,y,z,m,n,o,p,q,r] = undefined