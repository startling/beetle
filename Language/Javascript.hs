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
import Data.Traversable (Traversable, traverse)
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
-- bitraversable
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

data Expression v o
  = Variable v
  | Literal Text
  | Object [(Text, Expression v o)]
  | Attribute Text (Expression v o)
  | FunctionExp (Function v o)
  | Array [Expression v o]
  | Call (Expression v o) [Expression v o]
  | Assign (LHS v o) (Expression v o)
  | Other o
  deriving
  ( Eq
  , Ord
  , Show
  , Functor
  , Foldable
  , Traversable
  )

instance Bifunctor Expression where
  bimap = bimapDefault

instance Bifoldable Expression where
  bifoldMap = bifoldMapDefault

instance Bitraversable Expression where
  bitraverse f g (Variable v) = Variable <$> f v
  bitraverse _ _ (Literal t) = pure $ Literal t
  bitraverse f g (Object os) = Object <$> traverse
    (bitraverse pure $ bitraverse f g) os
  bitraverse f g (Attribute t e) = Attribute t <$> bitraverse f g e
  bitraverse f g (FunctionExp fn) = FunctionExp <$> bitraverse f g fn
  bitraverse f g (Array es) = Array <$> traverse (bitraverse f g) es
  bitraverse f g (Call e as) = Call <$> bitraverse f g e
    <*> traverse (bitraverse f g) as
  bitraverse f g (Assign l e) = Assign <$> bitraverse f g l
    <*> bitraverse f g e
  bitraverse _ g (Other o) = Other <$> g o

data Statement v o
  = Return (Expression v o)
  | Expression (Expression v o)
  deriving
  ( Eq
  , Ord
  , Show
  , Functor
  , Foldable
  , Traversable
  )

instance Bifunctor Statement where
  bimap = bimapDefault

instance Bifoldable Statement where
  bifoldMap = bifoldMapDefault

instance Bitraversable Statement where
  bitraverse f g (Return e) = Return <$> bitraverse f g e
  bitraverse f g (Expression e) = Return <$> bitraverse f g e

data LHS v o
  = LVariable v
  | LAttribute Text (Expression v o)
  deriving
  ( Eq
  , Ord
  , Show
  , Functor
  , Foldable
  , Traversable
  )

instance Bifunctor LHS where
  bimap = bimapDefault

instance Bifoldable LHS where
  bifoldMap = bifoldMapDefault

instance Bitraversable LHS where
  bitraverse f _ (LVariable v) = LVariable <$> f v
  bitraverse f g (LAttribute t e) = LAttribute t <$> bitraverse f g e

data Block v o = Block
  { provides   :: [v]
  , statements :: [Statement v o] }
  deriving
  ( Eq
  , Ord
  , Show
  , Functor
  , Foldable
  , Traversable
  )

instance Bifunctor Block where
  bimap = bimapDefault

instance Bifoldable Block where
  bifoldMap = bifoldMapDefault

instance Bitraversable Block where
  bitraverse f g (Block as ss) = Block <$> traverse f as
    <*> traverse (bitraverse f g) ss

data Function v o = Function
  { parameters :: [v]
  , body       :: Block v o
  }
  deriving
  ( Eq
  , Ord
  , Show
  , Functor
  , Foldable
  , Traversable
  )

instance Bifunctor Function where
  bimap = bimapDefault

instance Bifoldable Function where
  bifoldMap = bifoldMapDefault

instance Bitraversable Function where
  bitraverse f g (Function ps b) = Function <$> traverse f ps
    <*> bitraverse f g b

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
