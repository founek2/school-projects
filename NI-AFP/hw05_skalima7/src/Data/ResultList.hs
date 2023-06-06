module Data.ResultList where

import Data.List (sort, transpose)
import Data.Semigroup

data ResultList a = Results [a] | Error Int String
  deriving (Show, Read, Eq)

-- | Extract results to a list or return error with message
-- TODO
toList :: ResultList a -> [a]
toList (Results a) = a
toList (Error c b) = error ("#" ++ show c ++ ": " ++ b)

instance Semigroup (ResultList a) where
  -- \| Merge two result lists together
  -- TODO
  (<>) (Results a) (Results b) = Results $ a ++ b
  (<>) e1@(Error n1 _) e2@(Error n2 _) = if n1 >= n2 then e1 else e2
  (<>) e1 _ = e1

instance Monoid (ResultList a) where
  -- TODO
  mempty = Results []
  mappend = (<>)

instance Functor ResultList where
  -- \| Apply function over sorted list
  fmap fn (Results c) = Results (map fn c)

instance Applicative ResultList where
  -- TODO
  pure a = Results [a]

  -- \| Apply all functions to elements in result list
  -- TODO
  -- (<*>) (Results fn) (Results a) = Results (map (\x ->  foldl (\v f -> f v) x fn)  a)
  (<*>) (Results fn) (Results a) = Results (concat $ transpose $ map (\v -> map (\f -> f v) fn) a)
  (<*>) _ (Error n e) = Error n e

instance Monad ResultList where
  -- \| Apply on result list if valid and not empty
  -- TODO
  (>>=) (Results []) fn = Results []
  (>>=) (Results (x : xs)) fn = case fn x of
    Results a -> Results a <> (Results xs >>= fn)
    Error n e -> Error n e
  (>>=) (Error n e) fn = Error n e
