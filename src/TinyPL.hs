{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module TinyPL where

import Data.Generics
import Data.List (intercalate)

import Data.Hashable
import qualified GHC.Generics as GHCG

type Program = [Function]
data Function = Function String [String] Stmt
  deriving (Show, Data, Typeable, GHCG.Generic)

instance Hashable Function

data LExpr = Num Int
          | Var String
          | Neg LExpr
          | FunCall String [LExpr]
          | Add  LExpr LExpr
          | Sub  LExpr LExpr
          | Mult LExpr LExpr
          | Div  LExpr LExpr
          deriving (Show, Data, Typeable, GHCG.Generic)

instance Hashable LExpr

data Stmt = Assignment String LExpr
          | If LExpr Stmt
          | IfElse LExpr Stmt Stmt
          | Block [Stmt]
          | While LExpr Stmt
          | Return LExpr
          deriving (Show, Data, Typeable, GHCG.Generic)

instance Hashable Stmt

class ShowF a where
  showF :: a -> String

instance {-# OVERLAPPABLE #-} Show a => ShowF a where
  showF x = show x

instance {-# OVERLAPPABLE #-} ShowF a => ShowF [a] where
  showF xs = concatMap ((<> "\n") . showF) xs

instance {-# OVERLAPS #-} ShowF Program where
  showF = intercalate "\n" . map showF

instance ShowF Function where
  showF (Function f args b) =
    f <> "(" <> args' <> ") " <> (unlines $ toLines b)
    where args' = intercalate ", " $ args

instance ShowF LExpr where
  showF (Num x) = show x
  showF (Var x) = x
  showF (Neg e) = "-(" <> showF e <> ")"
  showF (FunCall f args) = f <> "(" <> args' <>")"
    where args' = intercalate ", " $ map showF args
  showF (Add e1 e2) = showF e1 <> " + " <> showF e2
  showF (Sub e1 e2) = showF e1 <> " - " <> showF e2
  showF (Mult e1 e2) = showF e1 <> " * " <> showF e2
  showF (Div e1 e2) = showF e1 <> " / " <> showF e2

instance ShowF Stmt where
  showF (Assignment v e) = v <> " = " <> showF e
  showF (If c b) = "if (" <> showF c <> ")" <> b'
    where b' = "\n  " <> showF b <> "\n"
  showF (IfElse c b1 b2) =
    "if (" <> showF c <> ")" <> b1' <> "else" <> b2'
    where b1' = "\n  " <> showF b1 <> "\n"
          b2' = "\n  " <> showF b2 <> "\n"
  showF (Block xs) = "{\n" <> xs' <> "}"
    where xs' = unlines $ map (("  " <>) . showF) xs
  showF (While c b) = "while (" <> showF c <> ")" <> b'
    where b' = "\n  " <> showF b <> "\n"
  showF (Return e) = "return " <> showF e


indent :: Int -> [String] -> [String]
indent n xs = map (sp <>) xs
  where sp = replicate n ' '

toLines :: Stmt -> [String]
toLines (Assignment v e) = [v <> " = " <> showF e]
toLines (If c b) = ("if (" <> showF c <> ")") : indent 2 (toLines b)
toLines (IfElse c b1 b2) =
  [ "if (" <> showF c <> ")" ]
  <> indent 2 (toLines b1)
  <> [ "else" ]
  <> indent 2 (toLines b2)
toLines (Block xs) = ["{"] <> indent 2 (concatMap toLines xs) <> ["}"]
toLines (While c b) =
  ["while (" <> showF c <> ")"]
  <> indent 2 (toLines b)
toLines (Return e) = ["return " <> showF e]
