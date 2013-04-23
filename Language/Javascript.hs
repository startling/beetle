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
