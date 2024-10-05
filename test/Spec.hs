{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.Hspec
import Test.Validity
import TrieModule
import Prelude hiding (filter, words)

-- Property-based tests for `Trie` operations
spec :: Spec
spec = do
  describe "TrieModule" $ do
    -- Validity property tests
    it "ensures that empty Trie is valid" $
      shouldBeValid (empty :: Trie Char)

    it "ensures that insert maintains validity" $
      forAllValid $ \(word :: String) ->
        forAllValid $ \trie ->
          shouldBeValid (TrieModule.insert word trie)

    it "ensures that remove maintains validity" $
      forAllValid $ \(word :: String) ->
        forAllValid $ \trie ->
          shouldBeValid (remove word trie)

    it "ensures that filter maintains validity" $
      forAllValid $ \(trie :: Trie Char) ->
        shouldBeValid (TrieModule.filter (\w -> length w > 2) trie)

    it "ensures that toList maintains validity" $
      forAllValid $ \(trie :: Trie Char) ->
        shouldBeValid (toList trie)

    it "ensures that fromList maintains validity" $
      forAllValid $ \words ->
        shouldBeValid (fromList (words :: [String]))

    it "ensures that member maintains validity" $
      forAllValid $ \(word :: String) ->
        forAllValid $ \trie ->
          shouldBeValid (member word trie)

    it "ensures that trie has a neutral element that doesn't affect the trie when inserted" $
      forAllValid $ \trie ->
        insert "" trie `shouldBe` trie

-- Helper to run the tests using hspec
main :: IO ()
main = hspec spec
