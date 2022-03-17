module ReturnChecker where

import TinyPL

import Data.Hashable
import CG
import Expr

checkProgram :: Program -> Bool
checkProgram = foldr1 (&&) . map checkFunction

checkFunction :: Function -> Bool
checkFunction (Function _ _ body) = checkStmt body

checkStmt :: Stmt -> Bool
checkStmt (Assignment _ _) = False
checkStmt (If _ b) = checkStmt b
checkStmt (IfElse _ t e) = checkStmt t && checkStmt e
checkStmt (Block xs) = foldr1 (||) $ map checkStmt xs
checkStmt (While _ s) = checkStmt s
checkStmt (Return _) = True

-- checkProgramCG :: Program -> CG Bool
-- checkProgramCG prog = mkCGNode (Lift True) combine deps
--   where deps = map checkFunctionCG prog
--         combine cs _ = foldr (liftOp (&&)) (Lift True) cs
--
-- checkFunctionCG :: Function -> CG Bool
-- checkFunctionCG (Function _ _ body) =
--   checkStmtCG body
--
-- checkStmtCG :: Stmt -> CG Bool
-- checkStmtCG (Assignment _ _) = mkCGLeaf False
-- checkStmtCG (If _ b) = checkStmtCG b
-- checkStmtCG (IfElse _ t e) = mkCGNode (Lift True) combine deps
--   where deps = map checkStmtCG [t, e]
--         combine [m, n] _ = liftOp (&&) m n
-- checkStmtCG (Block xs) = mkCGNode (Lift True) combine deps
--   where deps = map checkStmtCG xs
--         combine cs _ = foldr (liftOp (||)) (Lift True) cs
-- checkStmtCG (While _ s) = checkStmtCG s
-- checkStmtCG (Return _) = mkCGLeaf True

checkProgramC :: Program -> ExprC Bool
checkProgramC fs = do
  rs <- mapM checkFunctionC fs
  pure $ foldr1 (&&) rs

checkFunctionC :: Function -> ExprC Bool
checkFunctionC (Function name _ body) = labelC name $ checkStmtC body

checkStmtC :: Stmt -> ExprC Bool
checkStmtC s@(Return _) = labelC (hashS s) $ pure True
checkStmtC s@(Assignment _ _) = labelC (hashS s) $ pure False
checkStmtC s@(If _ b) =
  labelC (hashS s) $ checkStmtC b
checkStmtC s@(IfElse _ t e) =
  labelC (hashS s) $
    (&&) <$> checkStmtC t <*> checkStmtC e
checkStmtC s@(While _ b) = labelC (hashS s) $ checkStmtC b
checkStmtC s@(Block xs) = labelC (hashS s) $ do
  xs' <- mapM checkStmtC xs
  pure $ foldr1 (||) xs'

hashS :: Hashable a => a -> String
hashS = show . hash
