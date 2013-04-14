{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language OverloadedStrings #-}
module Language.Javascript where
-- base
import Control.Applicative
import Data.Char
import Data.List
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
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

data Expression v
  = Variable v
  | Literal Text
  | Object [(Text, Expression v)]
  | Attribute Text (Expression v)
  | FunctionExp (Function v)
  | Array [Expression Text]
  | Call (Expression v) [Expression v]
  deriving
  ( Eq
  , Ord
  , Show
  , Functor
  , Foldable
  , Traversable
  )

data Statement v
  = Return (Expression v)
  | Assign v (Expression v)
  | Expression (Expression v)
  deriving
  ( Eq
  , Ord
  , Show
  , Functor
  , Foldable
  , Traversable
  )

data Block v = Block
  { provides   :: [v]
  , statements :: [Statement v] }
  deriving
  ( Eq
  , Ord
  , Show
  , Functor
  , Foldable
  , Traversable
  )

data Function v = Function
  { parameters :: [v]
  , body       :: Block v
  }
  deriving
  ( Eq
  , Ord
  , Show
  , Functor
  , Foldable
  , Traversable
  )

type RenderT m = ReaderT Int (WriterT Builder m)
type Render = RenderT Identity

indented :: Monad m => RenderT m a -> RenderT m a
indented = local (+ 1)

line x = indent `liftM` ask >>= tell
  >> x >> tell "\n" where
    indent i = fromText $ T.replicate i "  "

word :: Monad m => Text-> RenderT m ()
word = tell .fromText

escape :: Text -> Text
escape = T.concatMap $ \x -> case x of
  '"' -> "\\\""; '\n' -> "\\n"; '\t' -> "\\t"; '\r' -> "\\r";
  '\b' -> "\\b"; '\f' -> "\\f"; '\v' -> "\\v"; '\0' -> "\\0";
  '\\' -> "\\\\";
  c -> if isPrint c then T.singleton c
    else T.pack $ printf "\\u%04x" $ ord c

expression :: Monad m => Expression Text -> RenderT m ()
expression (Variable t) = word t
expression (Literal l) = word $ "\"" <> escape l <> "\""
expression (Object []) = word "({})"
expression (Object o) = braces . indented $ mapM_ each o where
  braces :: Monad m => RenderT m a -> RenderT m ()
  braces b = word "({\n" >> b >> word "})"
  each :: Monad m => (Text, Expression Text) -> RenderT m ()
  each (k, v) = line (word $ "\"" <> escape k <> "\" : ") >> expression v
expression (Attribute t e) = expression e >> word "." >> word t
expression (Call f []) = expression f >> word "()"
expression (Call f (a : as)) = expression f
  >> parens (expression a >> forM_ as each) where
    parens :: Monad m => RenderT m a -> RenderT m ()
    parens b = word "(" >> b >> word ")"
    each :: Monad m => Expression Text -> RenderT m ()
    each x = word "," >> expression x
expression (FunctionExp f) = function f
expression (Array as) = word "[" >> commas as >> word "]" where
    commas :: Monad m => [Expression Text] -> RenderT m ()
    commas [] = word ""
    commas (a : []) = expression a
    commas (a : as) = expression a >> forM_ as
      (\e -> word ", " >> expression e)

statement :: Monad m => Statement Text -> RenderT m ()
statement (Assign v e) = line (word v >> word " = " >> expression e >> word ";")
statement (Return e) = line (word "return " >> expression e >> word ";")
statement (Expression e) = line (expression e >> word ";")

block :: Monad m => Block Text -> RenderT m ()
block (Block ps ss) = forM_ ps var >> mapM_ statement ss where
  var t = line $ word "var " >> word t >> word ";"

function :: Monad m => Function Text -> RenderT m ()
function (Function ps b) = parens $ do
    line $ word "function" >> parens (commas $ ps) >> word " {"
    indented $ block b
    word "}"
  where
    parens :: Monad m => RenderT m a -> RenderT m ()
    parens v = word "(" >> v >> word ")"
    commas :: Monad m => [Text] -> RenderT m ()
    commas [] = word ""
    commas (a : []) = word a
    commas (a : as) = word a >> forM_ as
      (\e -> word ", " >> word e)

runRenderT :: Monad m => RenderT m a -> m Builder
runRenderT r = execWriterT $ runReaderT r 0

runRender :: RenderT Identity a -> Builder
runRender = runIdentity . runRenderT

keywords :: [Text]
keywords =
  [ "break"
  , "case"
  , "catch"
  , "continue"
  , "debugger"
  , "default"
  , "delete"
  , "do"
  , "else"
  , "finally"
  , "for"
  , "function"
  , "if"
  , "in"
  , "instanceof"
  , "new"
  , "return"
  , "switch"
  , "this"
  , "throw"
  , "try"
  , "typeof"
  , "var"
  , "void"
  , "while"
  , "with"
  , "class"
  , "enum"
  , "export"
  , "extends"
  , "import"
  , "super"
  , "implements"
  , "interface"
  , "let"
  , "package"
  , "private"
  , "protected"
  , "public"
  , "static"
  , "yield"
  ]
