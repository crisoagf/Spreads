{-# LANGUAGE OverloadedStrings #-}
module Spread.CLI.NCurses where
import UI.NCurses
import Control.Monad

main :: IO ()
main = do
  runCurses $ do
    win <- defaultWindow
    updateWindow win $ do
      (h,w) <- windowSize
      moveCursor (2 * div h 20) (3 * div w 3)
      drawText "Lixo!"
    render
  void getContents
