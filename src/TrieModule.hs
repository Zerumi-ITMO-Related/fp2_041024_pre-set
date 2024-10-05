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

-- Definition of Trie data structure
data Trie = Node [(Char, Trie)] Bool deriving (Eq, Show)

-- Creates an empty Trie
empty :: Trie
empty = Node [] False

-- Inserts a word into the Trie
insert :: String -> Trie -> Trie
insert "" (Node children _) = Node children True
insert (ch:rest) (Node children isEnd) = Node (insert' ch rest children) isEnd
  where
    insert' x xs [] = [(x, insert xs empty)]
    insert' x xs ((c, t):ts)
      | x == c    = (c, insert xs t) : ts
      | otherwise = (c, t) : insert' x xs ts

-- Removes a word from the Trie
remove :: String -> Trie -> Trie
remove "" (Node children _) = Node children False
remove (ch:rest) (Node children isEnd) = Node (remove' ch rest children) isEnd
  where
    remove' _ _ [] = []
    remove' x xs ((c, t):ts)
      | x == c    = (c, remove xs t) : ts
      | otherwise = (c, t) : remove' x xs ts

-- Checks if a word is a member of the Trie
member :: String -> Trie -> Bool
member "" (Node _ isEnd) = isEnd
member (ch:rest) (Node children _) = member' ch rest children
  where
    member' _ _ [] = False
    member' x xs ((c, t):ts)
      | x == c    = member xs t
      | otherwise = member' x xs ts

-- Filters the Trie and returns a new Trie with only the words that satisfy the predicate
filter :: (String -> Bool) -> Trie -> Trie
filter f = filter' f ""
  where
    filter' :: (String -> Bool) -> String -> Trie -> Trie
    filter' p prefix (Node children isEnd) =
        Node filteredChildren (isEnd && p prefix)
      where
        filteredChildren = [(c, filteredSubTrie) | (c, childTrie) <- children,
                              let filteredSubTrie = filter' p (prefix ++ [c]) childTrie,
                              not (isEmpty filteredSubTrie)]

    -- Check if a Trie is empty (i.e., has no children and is not a word end)
    isEmpty :: Trie -> Bool
    isEmpty (Node [] False) = True
    isEmpty _               = False

-- Fold left function for Trie (accumulate full words)
_foldl :: (a -> String -> a) -> a -> Trie -> a
_foldl _f _acc = foldl' _f _acc ""
  where
    -- Helper function to accumulate the current word (prefix) as we traverse
    foldl' :: (a -> String -> a) -> a -> String -> Trie -> a
    foldl' f acc prefix (Node children isEnd) =
        let acc' = if isEnd then f acc prefix else acc
        in foldlChildren f acc' prefix children

    -- Traverse the children of the current node
    foldlChildren :: (a -> String -> a) -> a -> String -> [(Char, Trie)] -> a
    foldlChildren _ acc _ [] = acc
    foldlChildren f acc prefix ((c, t):ts) =
        let acc' = foldl' f acc (prefix ++ [c]) t
        in foldlChildren f acc' prefix ts

-- Fold right function for Trie (accumulate full words)
_foldr :: (String -> a -> a) -> a -> Trie -> a
_foldr _f _acc = foldr' _f _acc ""
  where
    -- Helper function to accumulate the current word (prefix) as we traverse
    foldr' :: (String -> a -> a) -> a -> String -> Trie -> a
    foldr' f acc prefix (Node children isEnd) =
        let acc' = foldrChildren f acc prefix children
        in if isEnd then f prefix acc' else acc'

    -- Traverse the children of the current node
    foldrChildren :: (String -> a -> a) -> a -> String -> [(Char, Trie)] -> a
    foldrChildren _ acc _ [] = acc
    foldrChildren f acc prefix ((c, t):ts) =
        let acc' = foldr' f acc (prefix ++ [c]) t
        in foldrChildren f acc' prefix ts

-- Map function for Trie
_map :: (String -> String) -> Trie -> Trie
_map f trie = fromList (Prelude.map f (toList trie))

-- Convert the Trie into a list of strings
toList :: Trie -> [String]
toList = _foldr (:) []

-- Create a Trie from a list of words
fromList :: [String] -> Trie
fromList = Prelude.foldr insert empty
