module Rdg where

import Expr

import Control.Applicative
import Text.Show.Functions

import Data.Maybe (fromJust)

type Op a = [Expr a] -> Expr a

data RDG a =
  RDGNode
  { cache :: Maybe a
  , op :: Expr a
  , dependencies :: [RDG a]
  }
  | RDGLeaf a
  deriving Show

isLeaf :: RDG a -> Bool
isLeaf (RDGLeaf _) = True
isLeaf _ = False

isNode :: RDG a -> Bool
isNode (RDGNode _ _ _) = True
isNode _ = False

leafValue :: RDG a -> a
leafValue (RDGLeaf x) = x
leafValue _ = undefined

instance Functor RDG where
  fmap f (RDGLeaf x) = RDGLeaf (f x)
  fmap f (RDGNode c g ds) =
    RDGNode (fmap f c) (fmap f g) (fmap (fmap f) ds)

mkRDGLeaf :: a -> RDG a
mkRDGLeaf = RDGLeaf

mkRDGNode :: (Maybe a) -> Expr a -> [RDG a] -> RDG a
mkRDGNode = RDGNode
