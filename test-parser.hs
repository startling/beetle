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
    it "parses blocks with assignments within" $ do
      unlines
        [ "% do"
        , "  % y := z"
        , "% end"
        ] `parsesTo` [Assignment "y" (Symbol "z")]
    it "parses blocks with calls within" $ do
      unlines
        [ "% do"
        , "  % x y"
        , "% end"
        ] `parsesTo` [Splice (Call (Symbol "x") (Symbol "y"))]
  describe "assignment" $ do
    let ?parser = sigil *> assignment
    it "parses assignment to symbols" $ do
      "% x := y" `parsesTo` ("x", Symbol "y")
    it "parses assignment to empty blocks" $ do
      "% x := do\n% end"`parsesTo` ("x", Block [])
  describe "abstract" $ do
    let ?parser = sigil *> abstract
    it "parses string literals" $ do
      "% \"abc\"" `parsesTo` Literal "abc"
    it "parses unary application" $ do
      "% x y" `parsesTo` Call (Symbol "x") (Symbol "y")
  describe "function" $ do
    let ?parser = sigil *> function
    it "parses empty functions" $ do
      unlines
        [ "% :x do"
        , "% end"
        ] `parsesTo` Fn "x" []
  describe "application" $ do
    let ?parser = application
    it "parses unary application" $ do
      "x y" `parsesTo` Call (Symbol "x") (Symbol "y")
    it "parses binary application" $ do
      "x y, z" `parsesTo` Call (Call (Symbol "x") (Symbol "y")) (Symbol "z")
    it "parses ternary application" $ do
      "x y, z, a" `parsesTo` Call
        (Call (Call (Symbol "x") (Symbol "y")) (Symbol "z")) (Symbol "a")
    it "parses nested application" $ do
      "x y z" `parsesTo` Call (Symbol "x") (Call (Symbol "y") (Symbol "z"))
    it "parses nested application with parens" $ do
      "x (y z)" `parsesTo` Call (Symbol "x") (Call (Symbol "y") (Symbol "z"))
