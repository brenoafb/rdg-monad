module GCG where

import Text.Show.Functions

import Expr

type Op a = [Expr a] -> Expr a -> Expr a

data GCG c a =
  GCGNode
  { cache :: c
  , nodeValue :: Expr a
  , op :: Op a
  , dependencies :: [GCG a c]
  }
  | GCGLeaf a
  deriving Show

isLeaf :: GCG c a -> Bool
isLeaf (GCGLeaf _) = True
isLeaf _ = False

isNode :: GCG c a -> Bool
isNode (GCGNode _ _ _ _) = True
isNode _ = False

leafValue :: GCG c a -> a
leafValue (GCGLeaf x) = x
leafValue _ = undefined

data GCGPath c a  = GCGTop
                 | GCGPathNode (c, Expr a, Op a)
                               [GCG c a]
                               (GCGPath c a)
                               [GCG c a]

data GCGLocation c a = GCGLocation (GCG c a) (GCGPath c a)

gcgGoLeft (GCGLocation cg p) = case p of
  GCGPathNode xs (l:ls) up rs ->
    GCGLocation l (GCGPathNode xs ls up (cg:rs))

cgGoRight (GCGLocation cg p) = case p of
  GCGPathNode xs ls up (r:rs) ->
    GCGLocation r (GCGPathNode xs (cg:ls) up rs)

cgGoUp (GCGLocation cg p) = case p of
  GCGPathNode (c, e, f) ls up rs ->
    GCGLocation cg' up
    where cg' = GCGNode c e f (reverse ls ++ [cg] ++ rs)

cgGoDown (GCGLocation cg p) = case cg of
  GCGNode c x f (d:ds) -> GCGLocation d p'
    where p'  = GCGPathNode (c, x, f) [] p ds
