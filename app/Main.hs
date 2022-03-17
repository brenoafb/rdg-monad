module Main where

import Lib
import Expr
import TinyPL
import Parser
import Text.Pretty.Simple (pPrint)
import qualified ReturnChecker as ReturnChecker
import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Data.Generics.Zipper
import Data.Generics
import Control.Applicative
import Data.Maybe
import qualified Graphics.Vty as V
import Control.Monad (void, forever)
import Brick.BChan

tokenCount :: [Int] -> ExprC Int
tokenCount [] = pure 0
tokenCount (x:xs) =
  let n = case x of
          0 -> 0
          _ -> 1
  in labelC "myLabel" $ (+ n) <$> tokenCount xs

xs = [0,1,2,3,0,4,5]

data Tree = Leaf Int
          | Node Tree Tree
          deriving Show

myTree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))

countLeaves :: Tree -> ExprC Int
countLeaves (Leaf _) = pure 1
countLeaves (Node x y) =
  labelC "myLabel" $
    (+) <$> countLeaves x <*> countLeaves y

-- ui :: Typeable a => Zipper a -> Widget ()
ui z =
    [ withBorderStyle unicode $
      borderWithLabel (str "Program") $
      (center (zipperUI z) <+> vBorder)
    ]

zipperUI :: Typeable a => Zipper a -> Widget ()
zipperUI z =
  fromMaybe (str "Error!")
            (
                 (pieceUI (getHole z :: Maybe LExpr))
             <|> (pieceUI (getHole z :: Maybe [LExpr]))
             <|> (pieceUI (getHole z :: Maybe Stmt))
             <|> (pieceUI (getHole z :: Maybe [Stmt]))
             <|> (pieceUI (getHole z :: Maybe [String]))
             <|> (pieceUI (getHole z :: Maybe Function))
             <|> (pieceUI (getHole z :: Maybe Program))
            )

pieceUI :: (ShowF a) => Maybe a -> Maybe (Widget n)
pieceUI x = (str . showF) <$> x

-- handleEvent :: Typeable a => Zipper a -> BrickEvent Name Tick ->
handleEvent z (VtyEvent (V.EvKey V.KUp []))     = continue $ fromMaybe z (up z)
handleEvent z (VtyEvent (V.EvKey V.KDown []))   = continue $ fromMaybe z (down z)
handleEvent z (VtyEvent (V.EvKey V.KRight []))  = continue $ fromMaybe z (right z)
handleEvent z (VtyEvent (V.EvKey V.KLeft []))   = continue $ fromMaybe z (left z)
handleEvent z (VtyEvent (V.EvKey (V.KChar 'q') []))  = halt z
handleEvent z (VtyEvent (V.EvKey V.KEsc []))  = halt z
handleEvent z _ = continue z

-- myApp :: App St CustomEvent ()
myApp =
    Brick.App { appDraw = ui
              , appChooseCursor = showFirstCursor
              , appHandleEvent = handleEvent
              , appStartEvent = return
              , appAttrMap = const $ attrMap V.defAttr []
              }

main :: IO ()
main = do
  inputFile <- readFile "programs/fibonacci.im"
  let program = parseStr inputFile
      buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  chan <- newBChan 10
  void $ customMain initialVty buildVty (Just chan) myApp (toZipper program)

  -- simpleMain (ui name programString)
