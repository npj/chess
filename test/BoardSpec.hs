module BoardSpec where

import Test.Hspec

import Chess.Board

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Board" $ do
    describe "newBoard" $ do
      it "sets up a new board" $ do
        let board =  "R N B Q K B N R\n\
                     \P P P P P P P P\n\
                     \. . . . . . . .\n\
                     \. . . . . . . .\n\
                     \. . . . . . . .\n\
                     \. . . . . . . .\n\
                     \p p p p p p p p\n\
                     \r n b q k b n r"
        toString newBoard `shouldBe` board
