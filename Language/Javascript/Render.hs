{-# Language OverloadedStrings #-}
module Language.Javascript.Render where
-- base
import Control.Applicative
import Data.Char
import Text.Printf
-- text 
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Builder
-- transformers
import Data.Functor.Identity
-- mtl
import Control.Monad.Reader
import Control.Monad.Writer
-- beetle
import Language.Javascript

type RenderT m = ReaderT Int (WriterT Builder m)
type Render = RenderT Identity

indented :: Monad m => RenderT m a -> RenderT m a
indented = local (+ 1)

indent :: Monad m => RenderT m ()
indent = ask >>= \i -> replicateM_ i (tell "  ")

newline :: Monad m => RenderT m ()
newline = word "\n"

word :: Monad m => Text-> RenderT m ()
word = tell .fromText

commas :: Monad m => [RenderT m a] -> RenderT m ()
commas [] = return ()
commas (a : []) = a >> return ()
commas (a : as) = a >> forM_ as (word ", " >>)

escape :: Text -> Text
escape = T.concatMap $ \x -> case x of
  '"' -> "\\\""; '\n' -> "\\n"; '\t' -> "\\t"; '\r' -> "\\r";
  '\b' -> "\\b"; '\f' -> "\\f"; '\v' -> "\\v"; '\0' -> "\\0";
  '\\' -> "\\\\";
  c -> if isPrint c then T.singleton c
    else T.pack $ printf "\\u%04x" $ ord c

expression :: Monad m => Expression Text Text -> RenderT m ()
expression (Variable t) = word t
expression (Literal l) = word $ "\"" <> escape l <> "\""
expression (Object []) = word "({})"
expression (Object o) = braces . indented $ commas $ each <$> o where
  braces :: Monad m => RenderT m a -> RenderT m ()
  braces b = word "({" >> b >> newline >> indent >> word "})"
  each :: Monad m => (Text, Expression Text Text) -> RenderT m ()
  each (k, v) = newline >> indent
    >> word ("\"" <> escape k <> "\" : ") >> expression v
expression (Attribute t e) = expression e >> word "." >> word t
expression (Call f []) = expression f >> word "()"
expression (Call f (a : as)) = expression f
  >> parens (expression a >> forM_ as each) where
    parens :: Monad m => RenderT m a -> RenderT m ()
    parens b = word "(" >> b >> word ")"
    each :: Monad m => Expression Text Text -> RenderT m ()
    each x = word "," >> expression x
expression (FunctionExp f) = function f
expression (Array as) = word "["
  >> commas (expression <$> as) >> word "]"
expression (Assign v e) = lhs v >> word " = " >> expression e where
    lhs :: Monad m => LHS Text Text -> RenderT m ()
    lhs (LVariable t) = word t
    lhs (LAttribute t e) = expression e >> word "." >> word t
expression (Other o) = word o

statement :: Monad m => Statement Text Text -> RenderT m ()
statement (Return e) = indent >> word "return "
  >> expression e >> word ";" >> newline
statement (Expression e) = indent >> expression e
  >> word ";" >> newline

block :: Monad m => Block Text Text -> RenderT m ()
block (Block ps ss) = forM_ ps var >> mapM_ statement ss where
  var t = indent >> word "var " >> word t >> word ";" >> newline

function :: Monad m => Function Text Text -> RenderT m ()
function (Function ps b) = parens $ do
    word "function" >> parens (commas $ word <$> ps) >> word "{"
    newline >> indented (block b)
    indent >> word "}"
  where
    parens :: Monad m => RenderT m a -> RenderT m ()
    parens v = word "(" >> v >> word ")"

runRenderT :: Monad m => RenderT m a -> m Builder
runRenderT r = execWriterT $ runReaderT r 0

runRender :: RenderT Identity a -> Builder
runRender = runIdentity . runRenderT
