module Beetle.Abstract where
-- text
import Data.Text (Text)

data Declaration
  = Declaration Text Abstract
  deriving
  ( Eq
  , Ord
  , Show
  )

data Abstract
  = Dict [(Text, Abstract)]
  | Call Abstract Abstract
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
  = Paragraph [Either Text Abstract]
  | Reassignment Text Abstract
  | Assignment Text Abstract
  | Splice Abstract
  | Line
  deriving
  ( Eq
  , Ord
  , Show
  )
