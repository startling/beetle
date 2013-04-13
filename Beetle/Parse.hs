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
    chars = oneOf "-_"
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
  <|> (try $ sigil *> (uncurry Assignment <$> assignment))
  <|> (try $ sigil *> (uncurry Reassignment <$> reassignment))
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
    connect (Left a : Left b : cs) = Left (a <> b) : connect cs
    connect (a : bs) = a : connect bs
  
assignment :: (Monad m, TokenParsing m) => m (Text, Expression)
assignment = (,) <$> identifier <* assign <*> expression

reassignment :: (Monad f, TokenParsing f) => f (Text, Expression)
reassignment = (,) <$> identifier <* reassign <*> expression

dictionary :: (Monad m, TokenParsing m) => m [(Text, Expression)]
dictionary = braces . commaSep $ (,) <$> identifier <* arrow <*> expression

function :: (Monad m, TokenParsing m) => m Expression
function = Fn <$> (char ':' *> identifier) <*> block

withAttribute :: (Monad f, TokenParsing f) => f Expression -> f Expression
withAttribute x = foldl Attribute <$> x
  <*> many (char '.' *> identifier)

application :: (Monad f, TokenParsing f) => f Expression
application = apply <$> expressionLine <*> commaSep1 expression where
  apply a (b: bs) = apply (Call a b) bs
  apply a [] = a

expression :: (Monad f, TokenParsing f) => f Expression
expression = try application <|> withAttribute (parens expression) <|> expression'

expression' :: (Monad f, TokenParsing f) => f Expression
expression' = withAttribute $
      Symbol <$> identifier
  <|> Literal . T.pack <$> stringLiteral
  <|> Block <$> block
  <|> Dict <$> dictionary
  <|> function

expressionLine :: (Monad f, TokenParsing f) => f Expression
expressionLine = parens expression <|> runUnlined expression'

dec :: (Monad f, TokenParsing f) => f Declaration
dec = sigil *> (uncurry Declaration <$> assignment)

