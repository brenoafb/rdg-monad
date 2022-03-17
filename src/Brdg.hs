{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Brdg where

import Text.Show.Functions

data Brdg a b c = forall w x y z .
  Node { nodeValue :: c
       , op :: (b -> c -> a)
       , dep1 :: Either a (Brdg b w x)
       , dep2 :: Either b (Brdg c y z)
       }

newtype Leaf a = Leaf a
  deriving (Eq, Show)

-- data Node a =
--   Node a (

--bcombine :: (b -> c -> a)
--         -> (Either b (forall w x . Brdg b w x))
--         -> (Either c (forall y z . Brdg c y z))
--         -> Brdg a b c
--bcombine f (Left x) (Left y) = Node (f x y) f (Left x) (Left y)
--bcombine f b1 b2 =
--  Node (f (nodeValue b1) (nodeValue b2))
--       f
--       b1
--       b2

-- bcombine' :: (b -> c -> a)
--           -> b
--           -> c
--           -> Brdg a b c
-- bcombine' f x y = Node (f x y) f (Left x) (Left y)

-- b1 = Leaf 2
-- b2 = Leaf 'a'
-- b3 = Node (2,'a') (,) b1 b2
