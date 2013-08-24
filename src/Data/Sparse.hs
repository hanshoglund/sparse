
{-# LANGUAGE GeneralizedNewtypeDeriving,
    ScopedTypeVariables,
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
        runSparse,
        runSparseT,
        runSparseT',
        withState,

        -- * Primitives
        -- stateP,
        -- mapStateP,
        -- mapInputP,
        headP,
        splitP,
        gateP,

        -- * Basic parsers
        char,         
        notChar,
        charIf,
        string,
        stringIf,
        space,
        integer,
        stringLiteral,

        -- * Combinators
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
import Data.Maybe -- DEBUG
import Data.String
import Data.Tree
import Data.Default
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

asSparse = id
asSparse :: Sparse a -> Sparse a

instance IsString (SparseT s Char String) where
    fromString = string

type Sparse = SparseT () Char

-- | 
-- Run a parser, returning the result.
-- 
runSparse :: Sparse a -> String -> Maybe a
runSparse p = runSparseT p ()

-- | 
-- Run a parser with a custom state, returning the result.
-- 
runSparseT :: SparseT s a b -> s -> [a] -> Maybe b
runSparseT = curry . fmap (fmap snd) . getPartialP . getSparseT

-- | 
-- Run a parser with a custom state.
--
-- This is the most general way to run a parser. It returns the final state,
-- remaining input and the result.
-- 
runSparseT' :: SparseT s a b -> s -> [a] -> Maybe (s, [a], b)
runSparseT' = curry . fmap (fmap untrip) . getPartialP . getSparseT
    where untrip ((a,b),c) = (a,b,c)

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

-- | Succeed based on predicate, but do not consume input.
--
--   The given function receives the /entire/ remaining input.
--
gateP :: (s -> [a] -> Bool) -> SparseT s a ()
gateP = SparseT . PartialP . gateP'


headP' :: (s -> a -> Bool) -> (s, [a]) -> Maybe ((s, [a]), a)
headP' p (s, [])     = Nothing
headP' p (s, (x:xs)) = if not (p s x) then Nothing else Just ((s, xs), x)

splitP' :: (s -> [a] -> Int) -> (s, [a]) -> Maybe ((s, [a]), [a])
splitP' p (s, []) = Nothing
splitP' p (s, ys) = let n = p s ys in if n < 1 then Nothing else Just ((s, drop n ys), take n ys)

gateP' :: (s -> [a] -> Bool) -> (s, [a]) -> Maybe ((s, [a]), ())
gateP' p (s, [])     = Nothing
gateP' p (s, xs)    = if not (p s xs) then Nothing else Just ((s, xs), ())


----------

ifState :: (s -> Bool) -> SparseT s a b -> SparseT s a b
ifState p x = gateP (\s _ -> p s) >> x

-- char :: Char -> Sparse Char

char c = charIf (== c)

notChar c = charIf (/= c)

-- charIf :: (Char -> Bool) -> Sparse Char
charIf p = headP (const p)

-- string :: String -> Sparse String
string s = stringIf (length s) (== s)

-- stringIf :: Int -> (String -> Bool) -> Sparse String
stringIf n p = splitP (\_ xs -> if p (take n xs) then n else 0)

----------

-- Use applicative optional
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

space   = many1 (charIf isSpace)
symbol  = many1 (charIf isAlphaNum)

integer :: SparseT s Char Integer
integer = read <$> many1 (charIf isDigit)

stringLiteral :: SparseT s Char String
stringLiteral = between (char '"') (char '"') $ many (notChar '"')

brackets = between (char '{') (char '}')
braces   = between (char '[') (char ']')

----------

-- Tests




test :: SparseT Int Char String
test = withState id id $ do
    ifState (== 0) $ string "name:"
    optional space        
    n <- symbol
    m <- withState (+ 10) (subtract 10) stateP
    optional space
    many1 (string ";")
    optional space
    return ("Name is " ++ n ++ ", state is " ++ show m)


data JSON
    = Object [(String, JSON)]
    | Array [JSON]
    | String String
    | Number Double
    | Boolean Bool
    | Null
    deriving (Eq, Ord, Show)

json :: SparseT s Char JSON
json = empty
    <|> (Object                   <$> members)
    <|> (Array                    <$> elements)
    <|> (String                   <$> stringLiteral)
    <|> ((Number . fromIntegral)  <$> integer)
    <|> (const (Boolean False)    <$> string "false")
    <|> (const (Boolean True)     <$> string "true")
    <|> (const Null               <$> string "null")     
    where  
        members  = brackets (pair `sepBy` char ',')
        elements = braces (value `sepBy` char ',')

        pair  = do
            n <- stringLiteral
            optional space
            string ":"
            optional space
            v <- json
            return (n, v)
            
        value = json

----------

type Duration = Rational

data Rhythm a
    = Beat       Duration a                     -- d is divisible by 2
    | Group      [Rhythm a]                     -- normal note sequence
    | Dots       Int (Rhythm a)                 -- n > 0.
    | Tuplet     Duration (Rhythm a)            -- d is an emelent of 'tupletMods'.
    deriving (Eq, Show, Functor, Foldable)

rhTree :: Show a => Rhythm a -> Tree String
rhTree = go
    where
        go (Beat d a)   = Node (show d{- ++ ":" ++ show a-}) []
        go (Group as)   = Node "" (fmap rhTree as)
        go (Dots n a)   = Node ("dot:" ++ show n) [rhTree a]
        go (Tuplet d a) = Node ("tuplet:" ++ show d) [rhTree a]
        -- (realToFrac d :: Double)

putRh :: Show a => Maybe (Rhythm a) -> IO () 
putRh = putStrLn . drawTree . rhTree . fromMaybe (error "Could not quantize")

instance Semigroup (Rhythm a) where
    (<>) = mappend
instance Monoid (Rhythm a) where
    mempty = Group []
    Group as `mappend` Group bs   =  Group (as <> bs)
    r        `mappend` Group bs   =  Group ([r] <> bs)
    Group as `mappend` r          =  Group (as <> [r])

type Quant s a = SparseT s (Duration, a) (Rhythm a)

data QuantState = QuantState {
        timeMod_ :: Duration,  
        recur_   :: Int
    }
instance Default QuantState where def = QuantState {
        timeMod_ = 1,
        recur_   = 0
    }

class HasTimeScale a where                          
    getTimeScale :: a -> Duration
    mapTimeScale :: (Duration -> Duration) -> a -> a
instance HasTimeScale () where
    mapTimeScale f  = id          
    getTimeScale () = 1       
instance HasTimeScale QuantState where
    getTimeScale = timeMod_
    mapTimeScale f (QuantState tm r) = QuantState (f tm) r

class HasRecur a where
    getRecur :: a -> Int
    mapRecur :: (Int -> Int) -> a -> a
    recur, unrecur :: a -> a
    recur = mapRecur succ
    unrecur = mapRecur pred
    guardRecur :: SparseT a m n -> SparseT a m n
    guardRecur = ifState (\x -> getRecur x < 5)
instance HasRecur QuantState where
    getRecur                     = recur_
    mapRecur f (QuantState tm r) = QuantState tm (f r)

testQuant :: Quant QuantState () -> [Duration] -> Maybe (Rhythm ())
testQuant p = quant p . (`zip` repeat ())

quant :: Default s => Quant s a -> [(Duration, a)] -> Maybe (Rhythm a)
quant p = quant' p def

quant' :: Quant s a -> s -> [(Duration, a)] -> Maybe (Rhythm a)
quant' = runSparseT




allDivs :: (HasTimeScale s, HasRecur s) => Quant s a -> Quant s a
allDivs x = msum $ fmap (`scaleTime` x) divs
    where
        divs = [8,4,2,1] ++ fmap (recip.(2^)) [1..5] :: [Rational]


-- Tries to match 1, then shorter
rh4 :: (HasTimeScale s, HasRecur s) => Quant s a
rh4 = withState recur unrecur $ guardRecur $ empty
    -- 1/4 1/2 1/4
    <|> (half (group [half rh4, rh4, half rh4]))

    -- 1/4 1/4 1/4 1/4
    <|> (quarter (group [rh4, rh4, rh4, rh4]))

    -- dotted figures
    <|> (half (group [rh3, half rh4]))
    <|> (half (group [half rh4, rh3]))

    -- 1/2 1/2
    <|> (half (group [rh4, rh4]))

    -- 1
    <|> note



-- Tries to match 1+1/2, then shorter
rh3 :: (HasTimeScale s, HasRecur s) => Quant s a
rh3 = withState recur unrecur $ empty
    -- 1+1/2
    <|> dot note

    -- 1/2 1/2 1/2
    <|> (triple (half (group [rh4, rh4, rh4])))

    -- 1 1/2
    <|> (group [unit rh4, half rh4])

    -- 1/2 1
    <|> (group [half rh4, unit rh4])


-- Tries to match something in scale 1.5
dot, unit, double, half, triple, quarter :: (HasTimeScale s, HasRecur s) => Quant s a -> Quant s a
dot = fmap (Dots 1) . scaleTime (3/2)
unit = scaleTime (2/2)
double = scaleTime (2/1)
half = scaleTime (1/2)
quarter = scaleTime (1/4)
triple = scaleTime (1/3)

-- Tries to match 1 as a note
note :: (HasTimeScale s, HasRecur s) => Quant s a
note = noteIf (\s d x -> d / getTimeScale s == 1)

scaleTime :: (HasTimeScale s, HasRecur s) => Duration -> Quant s a -> Quant s a
scaleTime n = withState 
    (mapTimeScale (* n)) 
    (mapTimeScale (/ n))

group :: [Quant s a] -> Quant s a
group xs = Group <$> sequence xs

-- Mathes a single note whose duration is simple
-- note :: Quant s a
-- note = noteIf (\s d x -> isDivisibleBy 2 d)



noteIf :: (s -> Duration -> a -> Bool) -> Quant s a
noteIf p = uncurry beat <$> headP (\s (d,x) -> p s d x)
    where
        beat :: Duration -> a -> Rhythm a
        beat d x = Beat d x
        





-- As it sounds
isDivisibleBy :: Duration -> Duration -> Bool
isDivisibleBy n = (== 0.0) . snd . properFraction . logBaseR (toRational n) . toRational

logBaseR :: forall a . (RealFloat a, Floating a) => Rational -> Rational -> a
logBaseR k n
    | isInfinite (fromRational n :: a)      = logBaseR k (n/k) + 1
logBaseR k n
    | isDenormalized (fromRational n :: a)  = logBaseR k (n*k) - 1
logBaseR k n                         = logBase (fromRational k) (fromRational n)


----------

first f (a, b) = (f a, b)
single x = [x]
list z f xs = case xs of
    [] -> z
    ys -> f ys

[a,b,c,d,e,f,g,x,y,z,m,n,o,p,q,r] = undefined