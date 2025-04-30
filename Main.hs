module Main where
import DotsBoxes
import ReadGame
import System.IO
import Testing

main :: IO ()
main = do
  content <- readFile "game3.txt"
  let board = readGame content
  print board
