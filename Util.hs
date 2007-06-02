{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module Util where

import Data.Maybe

{-
 - Existentials
 -}

data Some :: (* -> *) -> * where
	Exists :: forall a. t a -> Some t 

elim :: (forall a. t a -> b) -> Some t -> b
elim f (Exists t) = f t

elim2 :: (forall a b. t a -> t b -> c) -> Some t -> Some t -> c
elim2 f (Exists t) (Exists t') = f t t'

{-
 - Search monad
 -}

data Search f a b = Found (f a) b

class Monad (m a) => SearchMonad m a where
	found :: a -> m a a

fromSearch :: Search f a b -> f a
fromSearch (Found a _) = a

compareUsing :: (SearchMonad m a) => (a -> Bool) -> a -> m a a
compareUsing f a 
	| f a = found a 
	| otherwise = return a 

instance SearchMonad (Search Maybe) a where
	found a = Found (Just a) a

instance SearchMonad (Search []) a where
	found a = Found [a] a 

instance Monad (Search Maybe a) where
	return b = Found Nothing b
	Found (Just a) a' >>= f = case f a' of
		Found _ b -> Found (Just a) b
	Found Nothing a' >>= f = f a'

instance Monad (Search [] a) where
	return b = Found [] b
	Found as a' >>= f = case f a' of
		Found as' b -> Found (as ++ as') b

{-
 - Various
 -}

implies :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f `implies` g = \a -> not (f a) || g a

lookup' :: Eq a => a -> [(a,b)] -> b
lookup' key = fromJust . lookup key

-- Comma-separate a list of strings
flattenComma :: [String] -> String
flattenComma [] = ""
flattenComma [x] = x
flattenComma (x:xs) = x ++ ", " ++ flattenComma xs

{-
 - Monadic operators
 -}

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f [] = return []
concatMapM f (a:as) = do
	bs1 <- f a
	bs2 <- concatMapM f as
	return (bs1 ++ bs2)

fromJustM :: Monad m => String -> Maybe a -> m a
fromJustM err (Just a) = return a
fromJustM err Nothing = fail err
