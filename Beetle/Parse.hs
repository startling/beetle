module Beetle.Parse where
-- base
import Control.Applicative
import Control.Monad
import Data.Monoid
-- unoreded-containers
import Data.HashSet (fromList)
-- parsers
import Text.Parser.Char
import Text.Parser.Token
import Text.Parser.LookAhead
import Text.Parser.Combinators
import qualified Text.Parser.Token.Highlight as H
-- text
import Data.Text (Text)
import qualified Data.Text as T
-- beetle
import Beetle.Abstract

assign :: TokenParsing m => m String
assign = highlight H.Special $ symbol "="

reassign :: TokenParsing m => m String
reassign = highlight H.Special $ symbol ":="

arrow :: TokenParsing m => m String
arrow = highlight H.Special $ symbol "=>"

sigil :: TokenParsing m => m String
sigil = highlight H.Special $ symbol "%"

idStyle :: CharParsing m => IdentifierStyle m
idStyle = IdentifierStyle "identifier"
  (chars <|> letter) (chars <|> alphaNum)
  reserved H.Identifier H.Special where
    chars = oneOf "-_#"
    reserved = fromList ["do", "end"]

identifier :: (Monad f, TokenParsing f) => f Text
identifier = T.pack <$> ident idStyle

reserved :: (Monad m, TokenParsing m) => String -> m ()
reserved = reserve idStyle

block :: (Monad m, TokenParsing m) => m [Statement]
block = reserved "do" *> statements where
  end = sigil *> reserved "end"
  statements = [] <$ try end <|> (:) <$> statement <*> statements

statement :: (Monad f, TokenParsing f) => f Statement
statement = empty
  <|> (try $ sigil *> assignment)
  <|> (try $ sigil *> reassignment)
  <|> (try $ sigil *> (Splice <$> expression))
  <|> spaces *> (Paragraph <$> paragraph)

paragraph :: (Monad f, TokenParsing f) => f [Either Text Expression]
paragraph = connect <$> sepBy1
  ((Left . T.pack <$> line) <|> (Right <$> splice))
  (notFollowedBy sigil) where
    splice :: (Monad m, TokenParsing m) => m Expression
    splice = nesting $ between (char '`') (char '`') $ expression
    line :: CharParsing f => f [Char]
    line = some (noneOf "`\n") <* spaces
    -- concatenate adjacent Left Texts.
    connect :: [Either Text Expression] -> [Either Text Expression]
    connect [] = []
    connect (Left a : Left b : cs) = connect
      $ Left (a <> T.singleton ' ' <> b) : cs
    connect (a : bs) = a : connect bs

lhs = (LVariable <$> identifier) <|> lattribute where
  lattribute = expression >>= \x -> case x of
    (Attribute e t) -> pure $ LAttribute t e; _ -> empty;
  
assignment :: (Monad f, TokenParsing f) => f Statement
assignment = Assignment <$> (identifier <* assign) <*> expression

reassignment :: (Monad f, TokenParsing f) => f Statement
reassignment = Reassignment <$> (lhs <* reassign) <*> expression

dictionary :: (Monad m, TokenParsing m) => m [(Text, Expression)]
dictionary = braces . commaSep $ (,) <$> identifier <* arrow <*> expression

function :: (Monad m, TokenParsing m) => m Expression
function = Fn <$> (char ':' *> identifier) <*> block

attributes :: (Monad m, TokenParsing m)
  => (b -> Text -> b) -> m b -> m b
attributes f x = foldl f <$> x <*> many (char '.' *> identifier)

application :: (Monad f, TokenParsing f) => f Expression
application = apply <$> expressionLine <*> commaSep1 expression where
  apply a (b: bs) = apply (Call a b) bs
  apply a [] = a

expression :: (Monad f, TokenParsing f) => f Expression
expression = try application <|> attributes Attribute
  (parens expression) <|> expression'

expression' :: (Monad f, TokenParsing f) => f Expression
expression' = attributes Attribute $
      Symbol <$> identifier
  <|> Literal . T.pack <$> stringLiteral
  <|> Block <$> block
  <|> Dict <$> dictionary
  <|> function

expressionLine :: (Monad f, TokenParsing f) => f Expression
expressionLine = parens expression <|> runUnlined expression'

dec :: (Monad f, TokenParsing f) => f Declaration
dec = sigil *> (Declaration <$> identifier <* assign <*> expression)
