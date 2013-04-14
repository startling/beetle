{-# Language OverloadedStrings #-}
module Beetle.Compile where
-- text
import Data.Text (Text)
import qualified Data.Text as T
-- beetle
import qualified Beetle.Abstract as B
import Language.Javascript

-- | Create a top-level Javascript declaration from a Beetle one.
declaration :: Expression -> B.Declaration -> Statement
declaration e (B.Declaration t a) = Var (mangle t) . Just $ expression e a

-- | Create a Javascript function frxom a list of parameters and some
-- Beetle statements.
function ps ss = Function ("element" : map mangle ps)
  . onLast Return $
    map (statement $ Variable "element") ss where
      onLast _ [] = []
      onLast f (a : []) = f a : []
      onLast f (a : as) = a : onLast f as

-- | An 'Expression' representing the runtime function with some name.
runtime :: Text -> Expression
runtime = Attribute (Variable "beetle") . mangle

-- | Transform a valid Beetle identifier to a valid Javascript one.
mangle t = let m = T.concatMap each t in
  if m `elem` keywords then T.cons '$' m else m where
    each :: Char -> Text
    each c = case c of
      '-' -> "___"; '_' -> "_____";
      '#' -> "_______";
      c -> T.singleton c;
    
-- | Compile a Beetle expression to a Javascript one.
expression :: Expression -> B.Expression -> Expression
expression e (B.Symbol t) = if t `elem` functions
  then runtime t else Variable $ mangle t
  where
    functions :: [Text]
    functions = ["paragraph", "field", "debug-print"
      , "link", "exec", "switch-to", "if", "#t", "#f" ]
expression e (B.Attribute a s) = Attribute (expression e a) (mangle s)
expression e (B.Literal t) = Literal t
expression e (B.Call a b) = Call (expression e a) [e, expression e b]
expression e (B.Block ss) = function [] ss
expression e (B.Fn p ss) = function [p] [B.Splice $ B.Block ss]
expression e (B.Dict ss) = Object
  $ map (\(a, b) -> (mangle a, expression e b)) ss

-- | Compile a Beetle statement to a Javascript one.
statement :: Expression -> B.Statement -> Statement
statement e (B.Reassignment m as a) = Reassign (mangle m)
  (map mangle as) $ expression e a
statement e (B.Assignment m a) = Var (mangle m) . Just $ expression e a
statement e (B.Paragraph ts) = Expression
  $ Call (runtime "paragraph") . (e :) . return
    $ Call
      (Attribute
        (Array $ map (either Literal (expression e)) ts) "join")
      [Literal ""]
statement e (B.Splice x) = Expression $ expression e x
