module CG where

import Text.Show.Functions

import Expr

import Rdg

type CG a = RDG a

mkCGLeaf :: a -> CG a
mkCGLeaf = RDGLeaf

mkCGNode :: Expr a -> [CG a] -> CG a
mkCGNode = RDGNode Nothing

-- runCG :: CG a -> a
-- runCG = eval . cgExpr

-- cgExpr :: CG a -> Expr a
-- cgExpr cg
--   | isLeaf cg = Lift (leafValue cg)
--   | otherwise = f es e
--       where f = op cg
--             e = nodeValue cg
--             ds = dependencies cg
--             es = map cgExpr ds

-- cg2rdg :: CG a -> RDG a
-- cg2rdg cg
--   | isLeaf cg = mkRDGLeaf (leafValue cg)
--   | otherwise = mkRDGNode Nothing e f rdgs
--     where e = nodeValue cg
--           f = op cg
--           ds = dependencies cg
--           rdgs = map cg2rdg ds
cg2rdg = id
