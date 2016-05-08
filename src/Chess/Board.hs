module Chess.Board where

import Data.Char (toUpper, toLower)
import Data.List (find, intersperse)

newtype Board = Board { pieces :: [Piece] }
  deriving (Show)

data Col = A | B | C | D | E | F | G | H
  deriving (Enum, Show, Eq)

data Row = One | Two | Three | Four | Five | Six | Seven | Eight
  deriving (Enum, Show, Eq)

data Figure = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Show, Eq)

data Color = White | Black
  deriving (Show, Eq)

type Coord = (Col, Row)

data Piece = Piece { figure :: Figure
                   , color  :: Color
                   , coord  :: Coord
                   }
  deriving (Show)

class ToString a where
  toString :: a -> String

instance ToString Board where
  toString = boardToString

instance ToString Piece where
  toString = pieceToString

instance ToString Figure where
  toString = figureToString

newBoard :: Board
newBoard = Board $ setup White ++ setup Black

setup :: Color -> [Piece]
setup clr = backRow ++ frontRow
  where backRow     = zipWith3 Piece ranks colors (coords back )
        frontRow    = zipWith3 Piece pawns colors (coords front)
        ranks       = [ Rook, Knight, Bishop, Queen
                      , King, Bishop, Knight, Rook ]
        pawns       = replicate 8 Pawn
        colors      = replicate 8 clr
        coords row  = map (\col -> (col, row)) (enumFrom A)
        front       = if clr == White then Two else Seven
        back        = if clr == White then One else Eight

pieceAt :: Coord -> Board -> Maybe Piece
pieceAt (row, col) = find (hasCoord (row, col)) .  pieces

hasCoord :: Coord -> Piece -> Bool
hasCoord c = (== c) . coord

toGrid :: Board -> [[Maybe Piece]]
toGrid board = map row (enumFrom One)
  where row r   = map (col r) (enumFrom A)
        col r c = pieceAt (c, r) board

boardToString :: Board -> String
boardToString = concat . intersperse "\n" . map row . toGrid
  where row = concat . intersperse " " . map (maybe "." toString)

pieceToString :: Piece -> String
pieceToString piece = map format . toString . figure $ piece
  where format = case color piece of
          White -> toUpper
          Black -> toLower

figureToString :: Figure -> String
figureToString fig = case fig of
  Pawn   -> "p"
  Rook   -> "r"
  Knight -> "n"
  Bishop -> "b"
  Queen  -> "q"
  King   -> "k"
