{-# LANGUAGE DeriveGeneric #-}
module TrieModule
    ( Trie(..),
      empty,
      insert,
      remove,
      member,
      filter,
      _foldl,
      _foldr,
      _map,
      toList,
      fromList
    ) where

import Prelude hiding (filter)
import GHC.Generics (Generic)
import Data.Validity
import Test.Validity (GenValid)

-- Definition of Trie data structure
data Trie a = Node [(a, Trie a)] Bool deriving (Eq, Show, Generic)

instance (Validity a) => Validity (Trie a)
instance (GenValid a) => GenValid (Trie a)

-- Creates an empty Trie
empty :: Trie a
empty = Node [] False

-- Inserts a word into the Trie
insert :: Eq a => [a] -> Trie a -> Trie a
insert [] trie = trie
insert arr trie = insert' arr trie
  where
    insert' :: Eq a => [a] -> Trie a -> Trie a
    insert' [] (Node children _) = Node children True
    insert' (ch:rest) (Node children isEnd) = Node (insert'' ch rest children) isEnd
      where
        insert'' :: Eq a => a -> [a] -> [(a, Trie a)] -> [(a, Trie a)]
        insert'' x xs [] = [(x, insert' xs empty)]
        insert'' x xs ((c, t):ts)
          | x == c    = (c, insert' xs t) : ts
          | otherwise = (c, t) : insert'' x xs ts

-- Removes a word from the Trie
remove :: Eq a => [a] -> Trie a -> Trie a
remove [] (Node children _) = Node children False
remove (ch:rest) (Node children isEnd) = Node (remove' ch rest children) isEnd
  where
    remove' _ _ [] = []
    remove' x xs ((c, t):ts)
      | x == c    = (c, remove xs t) : ts
      | otherwise = (c, t) : remove' x xs ts

-- Checks if a word is a member of the Trie
member :: Eq a => [a] -> Trie a -> Bool
member [] (Node _ isEnd) = isEnd
member (ch:rest) (Node children _) = member' ch rest children
  where
    member' _ _ [] = False
    member' x xs ((c, t):ts)
      | x == c    = member xs t
      | otherwise = member' x xs ts

-- Filters the Trie and returns a new Trie with only the words that satisfy the predicate
filter :: ([a] -> Bool) -> Trie a -> Trie a
filter f = filter' f []
  where
    filter' :: ([a] -> Bool) -> [a] -> Trie a -> Trie a
    filter' p prefix (Node children isEnd) =
        Node filteredChildren (isEnd && p prefix)
      where
        filteredChildren = [(c, filteredSubTrie) | (c, childTrie) <- children,
                              let filteredSubTrie = filter' p (prefix ++ [c]) childTrie,
                              not (isEmpty filteredSubTrie)]

    -- Check if a Trie is empty (i.e., has no children and is not a word end)
    isEmpty :: Trie a -> Bool
    isEmpty (Node [] False) = True
    isEmpty _               = False

-- Fold left function for Trie (accumulate full words)
_foldl :: (a -> [b] -> a) -> a -> Trie b -> a
_foldl _f _acc = foldl' _f _acc []
  where
    -- Helper function to accumulate the current word (prefix) as we traverse
    foldl' :: (a -> [b] -> a) -> a -> [b] -> Trie b -> a
    foldl' f acc prefix (Node children isEnd) =
        let acc' = if isEnd then f acc prefix else acc
        in foldlChildren f acc' prefix children

    -- Traverse the children of the current node
    foldlChildren :: (a -> [b] -> a) -> a -> [b] -> [(b, Trie b)] -> a
    foldlChildren _ acc _ [] = acc
    foldlChildren f acc prefix ((c, t):ts) =
        let acc' = foldl' f acc (prefix ++ [c]) t
        in foldlChildren f acc' prefix ts

-- Fold right function for Trie (accumulate full words)
_foldr :: ([b] -> a -> a) -> a -> Trie b -> a
_foldr _f _acc = foldr' _f _acc []
  where
    -- Helper function to accumulate the current word (prefix) as we traverse
    foldr' :: ([b] -> a -> a) -> a -> [b] -> Trie b -> a
    foldr' f acc prefix (Node children isEnd) =
        let acc' = foldrChildren f acc prefix children
        in if isEnd then f prefix acc' else acc'

    -- Traverse the children of the current node
    foldrChildren :: ([b] -> a -> a) -> a -> [b] -> [(b, Trie b)] -> a
    foldrChildren _ acc _ [] = acc
    foldrChildren f acc prefix ((c, t):ts) =
        let acc' = foldr' f acc (prefix ++ [c]) t
        in foldrChildren f acc' prefix ts

-- Map function for Trie
_map :: Eq a => ([a] -> [a]) -> Trie a -> Trie a
_map f trie = fromList (Prelude.map f (toList trie))

-- Convert the Trie into a list of strings
toList :: Trie a -> [[a]]
toList = _foldr (:) []

-- Create a Trie from a list of words
fromList :: Eq a => [[a]] -> Trie a
fromList = Prelude.foldr insert empty
