module Intermediate.Syntax
  ( Atom
  , nil
  , wrap
  , unwrap
  , Scope
  , unbound
  , abstract
  , instantiate
  , count
  , BinOp (..)
  , BoolOp (..)
  , CompOp (..)
  , Target (..)
  , Operation (..)
  , Chain (..)
  , construct
  , consumes
  , Program (..)
  )
  where

import Util (add, sub)
import Data.Int (Int32)
import Data.List (elemIndex)
import Data.Functor ((<&>))
import Control.Monad.Reader (MonadReader (..), Reader, runReader, asks)
import GHC.Generics (Generic)

data Variable =
  Free String |
  Bound Int
  deriving (Show)

data Atom =
  Nil |
  Variable Variable
  deriving (Show)

nil :: Atom
nil = Nil

wrap :: String -> Atom
wrap = Variable . Free

unwrap :: Atom -> Maybe String
unwrap = \case
  Nil -> Nothing
  Variable (Free variable) -> Just variable
  Variable (Bound _) -> error "bound variable"

data Scope a = Scope Int a

instance Show a => Show (Scope a) where
  show (Scope quantity scope) = "{" ++ show quantity ++ " | " ++ show scope ++ "}"

unbound :: a -> Scope a
unbound = Scope 0

abstract :: Walk a => [String] -> a -> Scope a
abstract targets operation = Scope (length targets) $ with operation $ \case
  Free variable -> case variable `elemIndex` targets of
    Nothing -> return (Variable $ Free variable)
    Just index -> asks (Variable . Bound . add index)
  
  variable -> return (Variable variable)

instantiate :: Walk a => [Atom] -> Scope a -> a
instantiate atoms (Scope _ scope) = with scope $ \case
  Bound variable -> asks (sub variable) <&> \case
    index | index >= 0 -> atoms !! index
    _ -> Variable (Bound variable)

  variable -> return (Variable variable)

count :: Scope a -> Int
count (Scope quantity _) = quantity

data BinOp =
  Add |
  Sub |
  Mul |
  Div
  deriving (Show)

data BoolOp =
  And |
  Or
  deriving (Show)

data CompOp =
  Eq |
  Ne |
  Lt |
  Le |
  Gt |
  Ge
  deriving (Show)

data Target = Target
  { block :: String
  , atoms :: [Atom]
  }
  deriving (Show, Generic)

data Operation =
  Pure Atom |
  Jump Target |
  ClosureAlloc String [Atom] |
  ClosureEnter Atom [Atom] |
  StructAlloc [Atom] |
  StructSelect Atom Int32 |
  Int32Alloc Int32 |
  Int32If Atom Target Target |
  Int32Match Atom [(Int32, Target)] |
  Int32BinOp BinOp Atom Atom |
  Int32BoolOp BoolOp Atom Atom |
  Int32CompOp CompOp Atom Atom |
  Flt32Alloc Float |
  Flt32BinOp BinOp Atom Atom |
  Flt32CompOp CompOp Atom Atom
  deriving (Show)

data Chain = Chain
  { operation :: Operation
  , continuation :: Maybe (Scope Chain)
  }
  deriving (Show)

construct :: [(String, Operation)] -> Operation -> Chain
construct operations operation = foldr go (Chain operation Nothing) operations where
  go (name, body) continuation = Chain body (Just $ abstract [name] continuation)

class Consumes a where
  consumes :: a -> [String]

instance Consumes Atom where
  consumes = \case
    Nil -> []
    Variable (Free variable) -> [variable]
    Variable (Bound _) -> []

instance Consumes Target where
  consumes Target { atoms } = concatMap consumes atoms

instance Consumes Operation where
  consumes = \case
    Pure atom -> consumes atom
    Jump target -> consumes target
    ClosureAlloc _ atoms -> concatMap consumes atoms
    ClosureEnter _ atoms -> concatMap consumes atoms
    StructAlloc atoms -> concatMap consumes atoms
    StructSelect _ _ -> []
    Int32Alloc _ -> []
    Int32If _ _ _ -> []
    Int32Match _ _ -> []
    Int32BinOp _ _ _ -> []
    Int32BoolOp _ _ _ -> []
    Int32CompOp _ _ _ -> []
    Flt32Alloc _ -> []
    Flt32BinOp _ _ _ -> []
    Flt32CompOp _ _ _ -> []

instance Consumes Chain where
  consumes Chain { operation, continuation } =
    consumes operation ++ consumes continuation

instance Consumes a => Consumes (Maybe a) where
  consumes = maybe [] consumes

instance Consumes a => Consumes (Scope a) where
  consumes (Scope _ scope) = consumes scope

data Program = Program
  { blocks :: [(String, Scope Chain)]
  , closures :: [(String, Scope (Scope Chain))]
  }
  deriving (Show, Generic)

instance Semigroup Program where
  (<>) (Program blocks closures) (Program blocks' closures') =
    Program (blocks <> blocks') (closures <> closures')

instance Monoid Program where
  mempty = Program { blocks = [], closures = [] }

type Depth = Reader Int

class Walk a where
  walk :: (Variable -> Depth Atom) -> a -> Depth a

with :: Walk a => a -> (Variable -> Depth Atom) -> a
with subject action = runReader (walk action subject) 0

instance Walk Atom where
  walk action = \case
    Nil -> return Nil
    Variable variable -> action variable

instance Walk Target where
  walk action Target { block, atoms } = Target block <$> mapM (walk action) atoms

instance Walk Operation where
  walk action = \case
    Pure atom -> Pure <$> walk action atom
    Jump target -> Jump <$> walk action target
    ClosureAlloc name atoms -> ClosureAlloc name <$> mapM (walk action) atoms
    ClosureEnter atom atoms -> ClosureEnter <$> walk action atom <*> mapM (walk action) atoms
    StructAlloc atoms -> StructAlloc <$> mapM (walk action) atoms
    StructSelect atom index -> StructSelect <$> walk action atom <*> pure index
    Int32Alloc value -> pure (Int32Alloc value)
    Int32Match atom branches -> Int32Match <$> walk action atom <*> mapM (mapM $ walk action) branches
    Int32If atom truthy falsy -> Int32If <$> walk action atom <*> walk action truthy <*> walk action falsy
    Int32BinOp op left right -> Int32BinOp op <$> walk action left <*> walk action right
    Int32BoolOp op left right -> Int32BoolOp op <$> walk action left <*> walk action right
    Int32CompOp op left right -> Int32CompOp op <$> walk action left <*> walk action right
    Flt32Alloc value -> pure (Flt32Alloc value)
    Flt32BinOp op left right -> Flt32BinOp op <$> walk action left <*> walk action right
    Flt32CompOp op left right -> Flt32CompOp op <$> walk action left <*> walk action right

instance Walk Chain where
  walk action Chain { operation, continuation } =
    Chain <$> walk action operation <*> walk action continuation

instance Walk a => Walk (Maybe a) where
  walk action = \case
    Nothing -> pure Nothing
    Just scope -> Just <$> walk action scope

instance Walk a => Walk (Scope a) where
  walk action (Scope depth scope) = Scope depth <$> local (add depth) (walk action scope)
