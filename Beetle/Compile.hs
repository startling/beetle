{-# Language OverloadedStrings #-}
module Beetle.Compile where
-- text
import Data.Text (Text)
-- beetle
import qualified Beetle.Abstract as B
import Language.Javascript

declaration :: Expression -> B.Declaration -> Statement
declaration e (B.Declaration t a) = Var t . Just $ expression e a

expression :: Expression -> B.Abstract -> Expression
expression e (B.Symbol t) = Variable t
expression e (B.Literal t) = Literal t
expression e (B.Call a b) = Call (expression e a) [e, expression e b]
expression e (B.Block ss) = Function ["element"]
  $ map (statement $ Variable "element") ss
expression e (B.Fn p ss) = Function ["element", p]
  $ map (statement $ Variable "element") ss
expression e (B.Dict ss) = Object (map (fmap $ expression e) ss)

statement :: Expression -> B.Statement -> Statement
statement e (B.Reassignment m a) = Reassign m $ expression e a
statement e (B.Assignment m a) = Var m . Just $ expression e a
statement e (B.Chunk t) = Expression $ Call (Attribute e "append")
  [Call (Attribute (Variable "document") "createTextNode") [Literal t]]
statement e (B.Paragraph ts) = Expression $ Call (Attribute e "append")
 [ Call (Attribute (Variable "document") "createTextNode")
   [ foldr (Operator "+") (Literal "")
     $ map (either Literal $ expression e) ts
   ]
 ]
statement e (B.Line) = Expression $ Call (Attribute e "append")
  [Call (Attribute (Variable "document") "createTextNode") [Literal "\n"]]
statement e (B.Splice x) = Expression $ expression e x
