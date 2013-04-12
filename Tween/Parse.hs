module Tween.Parse where
-- base
import Control.Applicative
import Control.Monad
import Data.Monoid
-- unoreded-containers
import Data.HashSet (fromList)
-- parsers
import Text.Parser.Char
import Text.Parser.Token
import Text.Parser.Combinators
import qualified Text.Parser.Token.Highlight as H
-- text
import Data.Text (Text)
import qualified Data.Text as T
-- tween
import Tween.Abstract

equals :: TokenParsing m => m String
equals = highlight H.Special $ symbol ":="

arrow :: TokenParsing m => m String
arrow = highlight H.Special $ symbol "=>"

sigil :: TokenParsing m => m String
sigil = highlight H.Special $ symbol "%"

idStyle :: CharParsing m => IdentifierStyle m
idStyle = IdentifierStyle "identifier"
  letter alphaNum reserved H.Identifier H.Special where
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
  <|> (try $ sigil *> (Splice <$> abstract))
  <|> (try $ newline *> return Line)
  <|> (try $ spaces *> (Chunk . T.pack <$> many (noneOf "\n")) <* newline)

assignment :: (Monad m, TokenParsing m) => m (Text, Abstract)
assignment = (,) <$> identifier <* equals <*> abstract

dictionary :: (Monad m, TokenParsing m) => m [(Text, Abstract)]
dictionary = braces . commaSep $ (,) <$> identifier <* arrow <*> abstract

function :: (Monad m, TokenParsing m) => m Abstract
function = Fn <$> (char ':' *> identifier) <*> block

application :: (Monad f, TokenParsing f) => f Abstract
application = apply <$> abstract' <*> commaSep1 abstract where
  apply a (b: bs) = apply (Call a b) bs
  apply a [] = a

abstract :: (Monad f, TokenParsing f) => f Abstract
abstract = try application <|> abstract'

abstract' :: (Monad f, TokenParsing f) => f Abstract
abstract' = parens abstract
  <|> Symbol <$> identifier
  <|> Literal . T.pack <$> stringLiteral
  <|> Block <$> block
  <|> Dict <$> dictionary
  <|> function
-- TODO: function applications
