module Main ( 
      main,
      run
    ) where

import Prelude hiding (Left, Right)
import Prelude (getLine)
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)

-- instructions that can be read from 
data Instr =  Forward Float
              | Left Float
              | Right Float
              | Circle Float
              | Ellipse Float Float
              | Rect Float Float
              | CRect Float Float
              | Repeat Int [Instr]
              | Color String
                deriving (Show, Read)

-- not required, abstraction of a Logo program input and its conversion to a list of instructions
newtype Logo = Logo String deriving (Show, Read)
newtype Skell = Skell [Instr] deriving (Show, Read)

-- Shape : data that can be converted to an svg element
data Shape =  Seg Cursor Cursor String
              | EllipseShape Cursor Float Float String
              | RectangleShape Cursor Float Float String deriving (Show)

-- Cursor : contains state of the current position, orientation and color
data Cursor = Cursor {x::Float, y::Float, ang::Float, color::String} deriving (Show)


-- executes the following instruction in the instruction list
step :: Int -> [Instr] -> Cursor -> [Shape] -> (Cursor, [Shape], Int)
step index [] currentCursor shapes = (currentCursor, shapes, index) -- out of instructions
step index (instr:sequel) currentCursor@(Cursor x y ang _) shapes = case instr of -- x is instruction

  -- if is a control instruction (only repeat here)
  Repeat count repeatInstruction -> step index ((multLst count repeatInstruction) ++ sequel) currentCursor shapes
  Color color                    -> step (index+1) sequel (Cursor x y ang color) shapes

  _ -> -- if is an action instruction, execute it
    step (index+1) sequel newCursor newShapes
    where (newCursor, newShapes) = execInstr currentCursor instr shapes

-- Movement function selector
execInstr :: Cursor -> Instr -> [Shape] -> (Cursor, [Shape])
execInstr curs@(Cursor x y ang color) instr shapes =
  case instr of
    Forward dist  -> (new, shapes ++ [Seg curs new newCol]) where new@(Cursor _ _ _ newCol) = move curs instr
    Left turnVal  -> (Cursor x y (ang-turnVal) color, shapes)
    Right turnVal -> (Cursor x y (ang+turnVal) color, shapes)
    Circle radius -> (curs, shapes ++ [EllipseShape curs radius radius color])
    Ellipse rx ry -> (curs, shapes ++ [EllipseShape curs rx ry color])
    Rect w h      -> (curs, shapes ++ [RectangleShape curs w h color])
    CRect w h     -> (curs, shapes ++ [RectangleShape (Cursor (x-w/2) (y-h/2) ang color) w h color])
    _ -> (curs, shapes)

-- Movement
move :: Cursor -> Instr -> Cursor
move (Cursor currx curry ang color) instr@(Forward r) =
  Cursor (currx + r * cos(ang * pi / 180.0) ) (curry + r * sin(ang * pi / 180.0)) ang color

-- Utils (most of them aren't inline for understanding / clarity only)
-- converts string of format "[..., ..., ...]" with ... being readable Instr
logoToSkell :: Logo -> Skell
logoToSkell (Logo str) = 
  Skell $ (read str :: [Instr])

-- applies segToString to each Seg of Seg list/array
shapeArrayToSvgLines :: [Shape] -> [String]
shapeArrayToSvgLines = map shapeToString

-- convert Seg to an xml line using its cursors
shapeToString :: Shape -> String
shapeToString shape =
  case shape of
    Seg (Cursor x1 y1 _ _) (Cursor x2 y2 _ _) color   -> "\n<line x1=\"" ++ show x1 ++ "\" y1=\"" ++ show y1 ++ "\" x2=\"" ++ show x2 ++ "\" y2=\"" ++ show y2 ++ "\" stroke=\"" ++ color ++ "\" />"
    EllipseShape (Cursor x y ang _) rx ry color       -> "\n<ellipse cx=\"0\" cy=\"0\" rx=\"" ++ show rx ++ "\" ry=\"" ++ show ry ++ "\" style=\"fill:" ++ color ++ "\" transform=\"translate(" ++ show x ++ "," ++ show y ++ ") rotate(" ++ show ang ++ ")\" />"
    RectangleShape (Cursor x y ang _) w h color       -> "\n<rect y=\"0\" y=\"0\" width=\"" ++ show w ++ "\" height=\"" ++ show h ++ "\" style=\"fill:" ++ color ++ "\" transform=\"translate(" ++ show x ++ "," ++ show y ++ ") rotate(" ++ show ang ++ " " ++ show (w/2) ++ " " ++ show (h/2) ++ ")\" />"

-- replicate each element of a list : 5 * [0, 1] -> [0, 1, 0, 1, 0, 1, 0, 1, 0, 1]
multLst :: Int -> [a] -> [a]
multLst n arr = concat $ (concatMap . replicate) n [arr]

-- writes string to path (unused)
createAndWriteFile :: FilePath -> String -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path

  writeFile path content

run :: Skell -> (Float, Float) -> (Float, Float, Float, String) -> String
run (Skell instrLst) (width, height) (startX, startY, startAng, color) = do

  let (_, shapes, _) = step 0 instrLst (Cursor startX startY startAng color) []

  "<?xml version=\"1.0\" encoding=\"utf-8\"?><svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"" ++ show width ++ "\" height=\"" ++ show height ++ "\">" ++ concat (shapeArrayToSvgLines shapes) ++ "</svg>"


main :: IO()
main = do
  logoInput <- getLine
  size <- getLine
  startPos <- getLine
  
  --createAndWriteFile "./out/export.svg" $ run (logoToSkell $ Logo logoInput) (read size :: (Float, Float)) (read startPos :: (Float, Float, Float, String))
  putStrLn $ run (logoToSkell $ Logo logoInput) (read size :: (Float, Float)) (read startPos :: (Float, Float, Float, String))
