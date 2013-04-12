{-# Language ImplicitParams #-}
{-# Language OverloadedStrings #-}
module Main where
-- base
import Data.Foldable
import Data.Monoid
import Control.Applicative
-- hspec
import Test.Hspec
-- trifecta
import Text.Trifecta
-- tween
import Tween.Abstract
import Tween.Parse

parsesTo :: (?parser :: Parser a, Eq a) => String -> a -> Bool
a `parsesTo` b = toList (parseString ?parser mempty a) == [b]

main = hspec $ do
  describe "dictionary" $ do
    let ?parser = dictionary
    it "parses empty dictionaries" $ do
      "{}" `parsesTo` []
    it "parses singleton dictionaries" $ do
      "{a => b}" `parsesTo` [("a", Symbol "b")]
    it "parses two-item dictionaries" $ do
      "{a => b, c => d}" `parsesTo`
        [ ("a", Symbol "b")
        , ("c", Symbol "d")
        ]
  describe "block" $ do
    let ?parser = sigil *> block
    it "parses empty blocks" $ do
      "% do\n% end" `parsesTo` []
  describe "top" $ do
    let ?parser = top
    it "parses assignment to symbols" $ do
      "% x := y" `parsesTo` ("x", Symbol "y")
    it "parses assignment to empty blocks" $ do
      "% x := do\n% end"`parsesTo` ("x", Block [])
    it "parses assignment to blocks with assignments within" $ do
      unlines
        [ "% x := do"
        , "  % y := z"
        , "% end"
        ] `parsesTo` ("x", Block [Assignment "x" (Symbol "z")])

