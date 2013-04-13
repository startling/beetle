{-# Language OverloadedStrings #-}
module Language.Javascript where
-- base
import Data.Monoid
-- text
import Data.Text (Text)
import qualified Data.Text as T

data Expression
  = Literal Text
  | Variable Text
  | Number Rational
  | Array [Expression]
  | Attribute Expression Text
  | Object [(Text, Expression)]
  | Function [Text] [Statement]
  | Call Expression [Expression]
  | Operator Text Expression Expression
  deriving
  ( Eq
  , Ord
  , Show
  )

printExpression :: Expression -> Text
printExpression (Call e es) = "((" <> printExpression e <> ")("
  <> T.intercalate ", " (map printExpression es) <> "))"
printExpression (Function ps ss) = "function ("
  <> T.intercalate ", " ps <> "){\n"
  <> T.intercalate "\n" (map printStatement ss) <> "\n}"
printExpression (Object []) = "{}"
printExpression (Object ds) = "{\n    "
  <> T.intercalate "\n  , " (map line ds) <> "\n}" where
    line (a, b) = a <> " : " <> printExpression b
printExpression (Attribute e t) = "(" <> printExpression e <> ")." <> t
printExpression (Array _) = "[ \"todo\" ]"
printExpression (Number _) = "1337"
printExpression (Variable t) = t
printExpression (Literal t) = "\"" <> t <> "\"" -- escape!
printExpression (Operator t a b) = "((" <> printExpression a <> ") "
  <> t <> " (" <> printExpression b <> "))"

data Statement
  = Return Statement
  | Expression Expression
  | Reassign Text Expression
  | Var Text (Maybe Expression)
  deriving
  ( Eq
  , Ord
  , Show
  )

type Block = [Statement]

printStatement :: Statement -> Text
printStatement (Return e) = "return " <> printStatement e
printStatement (Expression e) = printExpression e <> ";"
printStatement (Reassign t e) = t <> " = " <> printExpression e <> ";"
printStatement (Var t Nothing) = "var " <> t <> ";"
printStatement (Var t (Just e)) = "var " <> t
  <> " = " <> printExpression e <> ";"
