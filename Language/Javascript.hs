{-# Language Rank2Types #-}
module Language.Javascript where
-- text
import Data.Text (Text)

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

data Statement
  = Expression Expression
  | Reassign Text Expression
  | Var Text (Maybe Expression)
  | If Expression Block [(Expression, Block)] (Maybe Block)
  deriving
  ( Eq
  , Ord
  , Show
  )

type Block = [Statement]
