{-# Language OverloadedStrings #-}
module Beetle.Compile where
-- text
import Data.Text (Text)
import qualified Data.Text as T
-- beetle
import qualified Beetle.Abstract as B
import Language.Javascript (Expression(..), LHS(..)
  , Statement(..), Block(..), Function(..), keywords)

data V
  = Passing
  | Introducing Text
  deriving
  ( Eq
  , Ord
  , Show
  )

data E
  = Element
  | Symbol Text
  | Runtime Text
  deriving
  ( Eq
  , Ord
  , Show
  )

-- | Create a top-level Javascript declaration from a set of Beetle ones.
declarations :: [B.Declaration] -> Block V E
declarations es = Block (map name es) (map assign es) where
  name :: B.Declaration -> V
  name (B.Declaration t _) = Introducing t
  assign :: B.Declaration -> Statement V E
  assign (B.Declaration t e) = Expression .
    Assign (LVariable (Introducing t)) $ expression e

-- | Compile an expression in Beetle to one in Javascript.
expression :: B.Expression -> Expression V E
expression (B.Attribute t e) = Attribute t $ expression e
expression (B.Dict os) = Object $ map (fmap expression) os
expression (B.Call e a) = Call (expression e) (return $ expression a)
expression (B.Fn t ss) = FunctionExp . Function [Passing, Introducing t]
  . Block [] . return . Return . FunctionExp
  . Function [Passing] $ block ss
expression (B.Block ss) = FunctionExp . Function [Passing] $ block ss
expression (B.Literal t) = Literal t
expression (B.Symbol v) = Other $ Symbol v

-- | Compile a statement in Beetle to some in Javascript.
statement :: B.Statement -> [Statement V E]
statement (B.Splice e) = [Expression $ expression e]
statement (B.Assignment t e) = return . Expression
  . Assign (LVariable (Introducing t)) $ expression e
statement (B.Reassignment l e) = return . Expression
  . Assign (lhs l) $ expression e where
    lhs (B.LSymbol t) = LVariable $ Introducing t
    lhs (B.LAttribute t e) = LAttribute t $ expression e
statement (B.Paragraph []) = return . Expression
  . Call (Other $ Runtime "paragraph") $ [Literal ""]
statement (B.Paragraph (e : [])) = return . Expression
  $ Call (Other $ Runtime "paragraph")
     [Other Element, either Literal expression e]
statement (B.Paragraph es) = return . Expression
  $ Call (Other $ Runtime "paragraph")
    [ Other Element
    , flip Call [Literal ""] . Attribute "join" . Array
      . (`map` es) $ either Literal expression
    ]

-- | Compile a Beetle block to a Javascript one.
block :: [B.Statement] -> Block V E
block ss = Block (ss >>= locals) (last $ ss >>= statement)
  where
    locals :: B.Statement -> [V]
    locals (B.Assignment t _) = [Introducing t]
    locals _ = []
    last :: [Statement v o] -> [Statement v o]
    last [] = []
    last (Expression e : []) = Return e : []
    last (a : as) = a : last as
    
element :: Expression Text a
element = Variable "element"

-- | An 'Expression' representing the runtime function with some name.
runtime :: Text -> Expression Text a
runtime = flip Attribute (Variable "beetle") . mangle

-- | A list of functions provided by the runtime.
provided :: [Text]
provided =
  [ "switch-to", "paragraph", "field"
  , "link", "exec", "if", "#t", "#f" ]

-- | Transform a valid Beetle identifier to a valid Javascript one.
mangle :: Text -> Text
mangle t = let m = T.concatMap each t in
  if m `elem` keywords then T.cons '$' m else m where
    each :: Char -> Text
    each c = case c of
      '-' -> "___"; '_' -> "_____";
      '#' -> "_______";
      c -> T.singleton c;
