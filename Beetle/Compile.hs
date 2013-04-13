{-# Language OverloadedStrings #-}
module Beetle.Compile where
-- text
import Data.Text (Text)
-- beetle
import qualified Beetle.Abstract as B
import Language.Javascript

-- | Create a top-level Javascript declaration from a Beetle one.
declaration :: Expression -> B.Declaration -> Statement
declaration e (B.Declaration t a) = Var t . Just $ expression e a

-- | Create a Javascript function from a list of parameters and some
-- Beetle statements.
function :: [Text] -> [B.Statement] -> Expression
function ps ss = Function ("element":ps) . onLast Return
  $ map (statement $ Variable "element") ss where
    onLast _ [] = []
    onLast f (a : []) = f a : []
    onLast f (a : as) = a : onLast f as

-- | An 'Expression' representing the runtime function with some name.
runtime :: Text -> Expression
runtime = Attribute (Variable "beetle")

-- | Compile a Beetle expression to a Javascript one.
expression :: Expression -> B.Abstract -> Expression
expression e (B.Symbol t) = case t of
    "switch-to" -> runtime "switch_to"
    "if" -> runtime "if_"
    t -> if t `elem` functions then runtime t else Variable t
  where
    functions :: [Text]
    functions = ["paragraph", "field", "link", "exec"]
    -- TODO: generalized transform to js-safe identifiers
expression e (B.Attribute a s) = Attribute (expression e a) s
expression e (B.Literal t) = Literal t
expression e (B.Call a b) = Call (expression e a) [e, expression e b]
expression e (B.Block ss) = function [] ss
expression e (B.Fn p ss) = function [p] ss
expression e (B.Dict ss) = Object (map (fmap $ expression e) ss)

-- | Compile a Beetle statement to a Javascript one.
statement :: Expression -> B.Statement -> Statement
statement e (B.Reassignment m a) = Reassign m $ expression e a
statement e (B.Assignment m a) = Var m . Just $ expression e a
statement e (B.Paragraph ts) = Expression $ Call (runtime "paragraph")
  [ e
  , foldr (Operator "+") (Literal "")
     $ map (either Literal $ expression e) ts
  ]
statement e (B.Splice x) = Expression $ expression e x
