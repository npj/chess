module Chess.Game where

import Chess.Board

data Game = Game { board :: Board }

newGame :: Game
newGame = Game { board = newBoard }
