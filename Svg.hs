module Main ( 
      main,
      run
    ) where

import Prelude hiding (Left, Right)
import Prelude (getLine)

-- instructions that can be read from 
data Instr =  Forward Float
              | Left Float
              | Right Float
              | Circle Float
              | Ellipse Float Float
              | Rect Float Float
              | CRect Float Float
              | Repeat Int [Instr]
              | To Float Float
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
step :: [Instr] -> Cursor -> [Shape] -> (Cursor, [Shape])
step [] currentCursor shapes = (currentCursor, shapes) -- out of instructions
step (instr:sequel) currentCursor@(Cursor x y ang color) shapes = case instr of -- x is instruction

  -- if is a control instruction (only repeat here)
  Repeat count repeatInstruction  -> step ((multLst count repeatInstruction) ++ sequel) currentCursor shapes
  Color newColor                  -> step sequel (Cursor x y ang newColor) shapes
  To tx ty                        -> step sequel (Cursor tx ty ang color) shapes

  _ -> -- if is an action instruction, execute it
    step sequel newCursor newShapes
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

-- applies segToString to each Seg of a Seg list/array
shapeArrayToSvgLines :: [Shape] -> [String]
shapeArrayToSvgLines = map shapeToString

-- convert Seg to an svg xml line using its cursors
shapeToString :: Shape -> String
shapeToString shape =
  case shape of
    Seg (Cursor x1 y1 _ _) (Cursor x2 y2 _ _) color   -> "\n<line x1=\"" ++ show x1 ++ "\" y1=\"" ++ show y1 ++ "\" x2=\"" ++ show x2 ++ "\" y2=\"" ++ show y2 ++ "\" stroke=\"" ++ color ++ "\" />"
    EllipseShape (Cursor x y ang _) rx ry color       -> "\n<ellipse cx=\"0\" cy=\"0\" rx=\"" ++ show rx ++ "\" ry=\"" ++ show ry ++ "\" style=\"fill:" ++ color ++ "\" transform=\"translate(" ++ show x ++ "," ++ show y ++ ") rotate(" ++ show ang ++ ")\" />"
    RectangleShape (Cursor x y ang _) w h color       -> "\n<rect y=\"0\" y=\"0\" width=\"" ++ show w ++ "\" height=\"" ++ show h ++ "\" style=\"fill:" ++ color ++ "\" transform=\"translate(" ++ show x ++ "," ++ show y ++ ") rotate(" ++ show ang ++ " " ++ show (w/2) ++ " " ++ show (h/2) ++ ")\" />"

-- replicate each element of a list : 5 * [0, 1] -> [0, 1, 0, 1, 0, 1, 0, 1, 0, 1]
-- applies concatMap to [arr] with partially evaluated function: replicate n    [[x,y,z]] => [[x,y,z],[x,y,z], ...(n times)]
-- then concats the result : [[x,y,z],[x,y,z], ...(n times)]
multLst :: Int -> [a] -> [a]
multLst n arr = concat $ (concatMap . replicate) n [arr]

run :: Skell -> (Float, Float) -> (Float, Float, Float, String) -> String
run (Skell instrLst) (width, height) (startX, startY, startAng, color) = do

  let (_, shapes) = step instrLst (Cursor startX startY startAng color) []

  "<?xml version=\"1.0\" encoding=\"utf-8\"?><svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"" ++ show width ++ "\" height=\"" ++ show height ++ "\">" ++ concat (shapeArrayToSvgLines shapes) ++ "</svg>"


main :: IO()
main = do
  logoInput <- getLine
  size <- getLine
  startPos <- getLine
  
  --createAndWriteFile "./out/export.svg" $ run (logoToSkell $ Logo logoInput) (read size :: (Float, Float)) (read startPos :: (Float, Float, Float, String))
  putStrLn $ run (logoToSkell $ Logo logoInput) (read size :: (Float, Float)) (read startPos :: (Float, Float, Float, String))
