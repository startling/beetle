{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language OverloadedStrings #-}
module Language.Javascript where
-- base
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Foldable (Foldable)
import Data.Traversable (Traversable, traverse)
import Text.Printf
-- transformers
import Data.Functor.Identity
-- text 
import Data.Text (Text)
-- bitraversable
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

class HasExpression x where
  expressions :: Applicative f
    => (Expression v o -> f (Expression v p))
    -> x v o -> f (x v p)

overExpressions :: HasExpression x =>
  (Expression v o -> Expression v p) -> x v o -> x v p
overExpressions f = runIdentity . expressions (pure . f)
  
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

instance HasExpression Expression where
  expressions = id

instance Applicative (Expression v) where
  pure = return
  (<*>) = ap

instance Monad (Expression v) where
  return = Other
  (>>=) = (join .) . flip fmap where
    join :: Expression v (Expression v o) -> Expression v o
    join (Other e) = e
    join (Variable v) = Variable v
    join (Literal t) = Literal t
    join (Object os) = Object $ map (fmap join) os
    join (Attribute t e) = Attribute t $ join e
    join (Array es) = Array $ map join es
    join (Call e as) = Call (join e) (map join as)
    join (Assign l e) = Assign (overExpressions join l) (join e)
    join (FunctionExp f) = FunctionExp $ overExpressions join f

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

instance HasExpression Statement where
  expressions f (Return e) = Return <$> f e
  expressions f (Expression e) = Expression <$> f e

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

instance HasExpression LHS where
  expressions f (LVariable v) = pure $ LVariable v
  expressions f (LAttribute t e) = LAttribute t <$> f e

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

instance HasExpression Block where
  expressions f (Block v ss) = Block v
    <$> traverse (expressions f) ss

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

instance HasExpression Function where
  expressions f (Function ps b) = Function ps <$> expressions f b

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
