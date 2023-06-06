{-# LANGUAGE AllowAmbiguousTypes #-}

module Lib where

data List a = Nil | Cons a (List a) deriving (Eq, Show, Read)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Foldable List where
  foldMap f Nil = mempty
  foldMap f (Cons a as) = mappend (f a) (foldMap f as)

instance Traversable List where
  traverse f Nil = pure Nil
  traverse f (Cons a as) = Cons <$> (f a) <*> traverse f as

neco :: (Num a) => a -> Maybe (List a)
neco a = (Cons <$> (Just a)) <*> (traverse Just (Cons 3 (Cons 1 Nil)))

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node l a r) = foldMap f l `mappend` f a `mappend` foldMap f r

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf a) = fmap Leaf (f a)
  traverse f (Node l a r) = (Node <$> traverse f l) <*> f a <*> traverse f r