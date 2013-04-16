{-# Language OverloadedStrings #-}
module Beetle.Compile where
-- text
import Data.Text (Text)
import qualified Data.Text as T
-- beetle
import qualified Beetle.Abstract as B
import Language.Javascript (Expression(..), LHS(..)
  , Statement(..), Block(..), Function(..), keywords)

-- | Create a top-level Javascript declaration from a set of Beetle ones.
declarations :: [B.Declaration] -> Block Text
declarations es = Block (map name es) (map assign es) where
  name :: B.Declaration -> Text
  name (B.Declaration t _) = t
  assign :: B.Declaration -> Statement Text
  assign (B.Declaration t e) = Expression .
    Assign (LVariable t) $ expression e

-- | Compile a Beetle Block to a Javascript one.
block :: [B.Statement] -> Block Text
block ss = Block (ss >>= locals) (last ret $ map statement ss) where
  locals (B.Assignment t _) = [t]
  locals otherwise = []
  statement :: B.Statement -> Statement Text
  statement (B.Splice e) = Expression $ expression e
  statement (B.Assignment t e) = Expression
    . Assign (LVariable t) $ expression e
  statement (B.Reassignment l e) = Expression . Assign
    (lhs l) $ expression e where
      lhs :: B.LHS -> LHS Text
      lhs (B.LVariable t) = LVariable $ mangle t
      lhs (B.LAttribute t e) = LAttribute (mangle t) (expression e)
  statement (B.Paragraph (e : [])) = Expression
    $ Call (runtime "paragraph")
      [element, either Literal expression e]
  statement (B.Paragraph es) = Expression
    $ Call (runtime "paragraph")
      [element, flip Call [Literal ""] . Attribute "join" . Array
        $ map (either Literal expression) es
      ]
  last :: (a -> a) -> [a] -> [a]
  last _ [] = []
  last f (b : []) = f b : []
  last f (b : bs) = b : last f bs
  ret :: Statement a -> Statement a
  ret (Expression e) = Return e
  ret otherwise = otherwise

expression :: B.Expression -> Expression Text
expression (B.Symbol t) = if t `elem` provided then
   runtime t else Variable $ mangle t
expression (B.Literal t) = Literal t
expression (B.Block b) = FunctionExp . Function ["element"] $ block b
expression (B.Fn a b) = FunctionExp . Function ["element", mangle a]
  . Block [] . return . Return  . FunctionExp
  . Function ["element"] $ block b
expression (B.Call a b) = Call (expression a) [element, expression b]
expression (B.Dict os) = Object $ map (fmap expression) os
expression (B.Attribute e t) = Attribute (mangle t) $ expression e

element :: Expression Text
element = Variable "element"

-- | An 'Expression' representing the runtime function with some name.
runtime :: Text -> Expression Text
runtime = flip Attribute (Variable "beetle") . mangle

-- | A list of functions provided by the runtime.
provided :: [Text]
provided =
  [ "switch-to", "paragraph", "field"
  , "link", "exec", "if" ]

-- | Transform a valid Beetle identifier to a valid Javascript one.
mangle :: Text -> Text
mangle t = let m = T.concatMap each t in
  if m `elem` keywords then T.cons '$' m else m where
    each :: Char -> Text
    each c = case c of
      '-' -> "___"; '_' -> "_____";
      '#' -> "_______";
      c -> T.singleton c;
