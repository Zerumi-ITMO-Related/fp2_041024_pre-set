module Main (main) where

import TrieModule

ourTrie :: Trie Char
ourTrie = insert "yay" $ insert "hello" empty

main :: IO ()
main = do
    print ourTrie
    print $ member "hello" ourTrie
    print $ member "world" ourTrie
    print $ member "yay" ourTrie
    print $ _map (const "looooool") ourTrie
    print $ toList ourTrie

