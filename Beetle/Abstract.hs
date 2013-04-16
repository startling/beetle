module Beetle.Abstract where
-- text
import Data.Text (Text)

data Declaration
  = Declaration Text Expression
  deriving
  ( Eq
  , Ord
  , Show
  )

data LHS
  = LSymbol Text
  | LAttribute Text Expression
  deriving
  ( Eq
  , Ord
  , Show
  )

data Expression
  = Attribute Text Expression
  | Dict [(Text, Expression)]
  | Call Expression Expression
  | Fn Text [Statement]
  | Block [Statement]
  | Literal Text
  | Symbol Text
  deriving
  ( Eq
  , Ord
  , Show
  )

data Statement
  = Paragraph [Either Text Expression]
  | Reassignment LHS Expression
  | Assignment Text Expression
  | Splice Expression
  deriving
  ( Eq
  , Ord
  , Show
  )
