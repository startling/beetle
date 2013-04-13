{-# Language ImplicitParams #-}
{-# Language OverloadedStrings #-}
module Main where
-- base
import Data.Foldable (toList)
import Data.Monoid
import Control.Applicative
-- hspec
import Test.Hspec
-- trifecta
import Text.Trifecta
-- beetle
import Beetle.Abstract
import Beetle.Parse

parsesTo :: (?parser :: Parser a, Eq a) => String -> a -> Bool
a `parsesTo` b = toList (parseString ?parser mempty a) == [b]

dictionaries :: Spec
dictionaries = describe "dictionary" $ do
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

blocks :: Spec
blocks = describe "block" $ do
  let ?parser = sigil *> block
  it "parses empty blocks" $ do
    "% do\n% end" `parsesTo` []
  it "parses blocks with assignments within" $ do
    unlines
      [ "% do"
      , "  % y = z"
      , "% end"
      ] `parsesTo` [Assignment "y" (Symbol "z")]
  it "parses blocks with calls within" $ do
    unlines
      [ "% do"
      , "  % x y"
      , "% end"
      ] `parsesTo` [Splice (Call (Symbol "x") (Symbol "y"))]
  it "parses things after paragraphs." $ do
    unlines
      [ "% do"
      , "  some text"
      , "  % a b"
      , "% end"
      ] `parsesTo`
      [ Paragraph [Left "some text"]
      , Splice (Call (Symbol "a") (Symbol "b"))
      ]
  it "parses paragraphs after calls correctly" $ do
    unlines
      [ "% do"
      , "  % x"
      , "  abc"
      , "% end"
      ] `parsesTo` [Splice (Symbol "x"), Paragraph [Left "abc"]]

assignments :: Spec
assignments = describe "assignment" $ do
  let ?parser = sigil *> assignment
  it "parses simple assignment" $ do
    "% x = y" `parsesTo` ("x", Symbol "y")

abstracts :: Spec
abstracts = describe "abstract" $ do
  let ?parser = sigil *> abstract
  it "parses string literals" $ do
    "% \"abc\"" `parsesTo` Literal "abc"
  it "parses unary application" $ do
    "% x y" `parsesTo` Call (Symbol "x") (Symbol "y")
  it "parses namespaced names correctly" $ do
    "% hello.world" `parsesTo` Attribute (Symbol "hello") "world"
  it "parses namedspace parenthesized things correctly" $ do
    "% (fn a).world" `parsesTo`
      Attribute (Call (Symbol "fn") (Symbol "a")) "world"

functions :: Spec
functions = describe "function" $ do
  let ?parser = sigil *> function
  it "parses empty functions" $ do
    unlines
      [ "% :x do"
      , "% end"
      ] `parsesTo` Fn "x" []

applications :: Spec
applications = describe "application" $ do
  let ?parser = sigil *> application
  it "parses unary application" $ do
    "% x y" `parsesTo` Call (Symbol "x") (Symbol "y")
  it "parses binary application" $ do
    "% x y, z" `parsesTo` Call (Call (Symbol "x") (Symbol "y")) (Symbol "z")
  it "parses ternary application" $ do
    "% x y, z, a" `parsesTo` Call
      (Call (Call (Symbol "x") (Symbol "y")) (Symbol "z")) (Symbol "a")
  it "parses nested application" $ do
    "% x y z" `parsesTo` Call (Symbol "x") (Call (Symbol "y") (Symbol "z"))
  it "parses nested application with parens" $ do
    "% x (y z)" `parsesTo` Call (Symbol "x") (Call (Symbol "y") (Symbol "z"))
  it "parses function calls taking two blocks" $ do
    unlines
      [ "% fn do"
      , "% end, do"
      , "% end"
      ] `parsesTo` Call (Call (Symbol "fn") (Block [])) (Block [])
  it "parses function calls taking anonymous functions" $ do
    unlines
      [ "% fn :x do"
      , "% end"
      ] `parsesTo` Call (Symbol "fn") (Fn "x" [])
  it "parses function calls taking blocks" $ do
    unlines
      [ "% fn do"
      , "% end"
      ] `parsesTo` Call (Symbol "fn") (Block [])

main :: IO ()
main = hspec $ sequence_
  [ dictionaries
  , blocks
  , assignments
  , abstracts
  , functions
  , applications
  ]
