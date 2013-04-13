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
printExpression (Object _) = "{ todo: \"this\" }"
printExpression (Attribute e t) = printExpression e <> "." <> t
printExpression (Array _) = "[ \"todo\" ]"
printExpression (Number _) = "1337"
printExpression (Variable t) = t
printExpression (Literal t) = "\"" <> t <> "\"" -- escape!

data Statement
  = Expression Expression
  | Reassign Text Expression
  | Var Text (Maybe Expression)
  deriving
  ( Eq
  , Ord
  , Show
  )

type Block = [Statement]

printStatement :: Statement -> Text
printStatement (Expression e) = printExpression e <> ";"
printStatement (Reassign t e) = t <> " = " <> printExpression e <> ";"
printStatement (Var t Nothing) = "var " <> t <> ";"
printStatement (Var t (Just e)) = "var " <> t
  <> " = " <> printExpression e <> ";"