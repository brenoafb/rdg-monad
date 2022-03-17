{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Expr where

import Text.Show.Functions
import Control.Applicative
import Control.Monad
import Data.Generics

-- HOAS
-- http://dev.stephendiehl.com/fun/evaluation.html

data Expr a where
  Lift :: a                       -> Expr a
  Tup  :: Expr a -> Expr b        -> Expr (a, b)
  Lam  :: (Expr a -> Expr b)      -> Expr (a -> b)
  App  :: Expr (a -> b) -> Expr a -> Expr b
  Fix  :: Expr (a -> a)           -> Expr a
  Cons :: Expr a -> Expr [a]      -> Expr [a]
  Nil  ::                            Expr [a]

eval :: Expr a -> a
eval (Lift v)    = v
eval (Tup e1 e2) = (eval e1, eval e2)
eval (Lam f)     = \x -> eval (f (Lift x))
eval (App e1 e2) = (eval e1) (eval e2)
eval (Fix f)     = (eval f) (eval (Fix f))
eval (Cons x es) = eval x : eval es
eval Nil = []

instance Show a => Show (Expr a) where
  show (Lift v) = show v
  show _ = "<expr>"

liftOp f e1 e2 = App (App (Lift f) e1) e2

instance Functor Expr where
  fmap f (Lift x) = Lift (f x)
  fmap f (App e1 e2) = App (Lift f) (App e1 e2)
  fmap f (Fix g) = App (Lift f) (Fix g)
  fmap f (Cons x xs) = undefined
  fmap f (Tup e1 e2) = undefined
  fmap f (Lam g) = undefined

instance Applicative Expr where
  pure = Lift
  (<*>) = App

instance Monad Expr where
  (Lift x) >>= f = f x
  e >>= f = App (Lift $ eval . f) e

myExpr = App (Lift (+ 1)) (Lift 1)
myExpr' = App (Lift (* 1)) (Lift 3)

myExpr'' = do
  v <- myExpr
  v' <- myExpr'
  pure [v, v']

data ExprC a where
  LabelC :: String -> ExprC a -> ExprC a
  LiftC :: a -> ExprC a
  AppC  :: Maybe b -> ExprC (a -> b) -> ExprC a -> ExprC b
  FixC  :: ExprC (a -> a) -> ExprC a

labelC = LabelC

instance Show (ExprC a) where
  show (LabelC l c) = l <> " {" <> show c <> "}"
  show (LiftC x) = "|"
  show (FixC f) = "F(" <> show f <> ")"
  show (AppC c e1 e2) = "[" <> cs <> "] (" <> show e1 <> ") (" <> show e2 <> ")"
    where cs = case c of
                 Nothing -> "_"
                 Just _ -> "*"

exprCDepth :: ExprC a -> Int
exprCDepth (LiftC _) = 1
exprCDepth (LabelC _ c) = exprCDepth c
exprCDepth (AppC _ e1 e2) = 1 + (max (exprCDepth e1) (exprCDepth e2))
exprCDepth (FixC e) = 1 + (exprCDepth e)

evalC :: ExprC a -> a
evalC (LiftC v) = v
evalC (AppC c e1 e2) =
  case c of
    Nothing -> (evalC e1) (evalC e2)
    Just x -> x
evalC (FixC f) = (evalC f) (evalC (FixC f))
evalC (LabelC _ e) = evalC e

instance Functor ExprC where
  fmap f (LabelC s e) = LabelC s (fmap f e)
  fmap f (LiftC x) = LiftC (f x)
  fmap f (AppC c e1 e2) = AppC Nothing (LiftC f) (AppC c e1 e2)
  fmap f (FixC g) = AppC Nothing (LiftC f) (FixC g)

instance Applicative ExprC where
  pure = LiftC
  (<*>) = AppC Nothing

instance Monad ExprC where
  (LiftC x) >>= f = f x
  e >>= f = AppC Nothing (LiftC $ evalC . f) e

propagateExprC :: ExprC a -> ExprC a
propagateExprC (LabelC l e) = LabelC l (propagateExprC e)
propagateExprC (AppC Nothing e1 e2) =
  let e1' = propagateExprC e1
      e2' = propagateExprC e2
      c = evalC (AppC Nothing e1' e2')
  in AppC (Just c) e1' e2'
propagateExprC x = x
