module Main where
import Data.Maybe

import Data.List.Split (splitOn)  

import Text.Read
import System.IO
import System.Environment
import Control.Monad
import System.Console.GetOpt
import DotsBoxes
import DotsBoxesSolver

data Flag = Winner | Depth String | MoveFlag String | Verbose | Interactive | Help deriving (Show, Eq)

options :: [OptDescr Flag]
options = [ Option ['w'] ["winner"]        (NoArg Winner)           "Print the best move."
          , Option ['d'] ["depth"]         (ReqArg Depth "<num>")   "Specifies a cutoff depth, defaults to 4."
          , Option ['m'] ["move"]          (ReqArg MoveFlag "<move>") "Print the result of the move."
          , Option ['v'] ["verbose"]       (NoArg Verbose)          "Print both the move and result of the move."
          , Option ['i'] ["interactive"]   (NoArg Interactive)      "Play a game against the computer."
          , Option ['h'] ["help"]          (NoArg Help)             "Print usage information and exit."
          ]

executableName = "gameplay"
defaultDepth = 3 --change these for your particular game.
--delete these four lines after you import your module!
-- to here

moveIO :: [Flag] -> Board -> IO ()
moveIO flags game = 
  case getMove flags of
    Just movefl -> 
        case makeMove game movefl of
          Just g -> if Verbose `elem` flags 
                    then do
                      putStrLn $ "Move: " ++ showLine movefl
                      putStrLn $ "Result: " ++ case whoWillWin g of
                                                Win p -> "Win for " ++ [p]
                                                Tie ps -> "Tie between " ++ ps
                      putStrLn (prettyPrint g)
                    else putStrLn (showGame g)
          Nothing -> putStrLn "Error: illegal move"
    Nothing -> putStrLn "Error: invalid move format"

showGoodMove :: Bool -> Int -> Board -> IO ()
showGoodMove False depth game = 
    let move = bestMove game
    in putStrLn (showLine move)
showGoodMove True depth game = 
    let move = bestMove game
        result = case makeMove game move of
                 Just g -> case whoWillWin g of
                             Win p -> "Win for " ++ [p]
                             Tie ps -> "Tie between " ++ ps
                 Nothing -> "Illegal move"
    in putStrLn $ (showLine move) ++ " (" ++ result ++ ")"
    
showBestMove :: Bool  -> Board -> IO ()
showBestMove False game = putStrLn (showLine (bestMove game)) 
showBestMove True game = putStrLn $ (showLine (bestMove game)) ++ " " ++ (show (whoWillWin game)) 

interactiveIO :: Int -> Board -> IO ()
interactiveIO depth game = putStrLn "Feature not implemented" -- entirely optional interactive mode

readMove :: String -> Maybe Move
readMove s = 
    let parts = splitOn "," s
    in case parts of
        [x,y,"H"] -> Just ((read x - 1, read y - 1), Horizontal)
        [x,y,"h"] -> Just ((read x - 1, read y - 1), Horizontal)
        [x,y,"V"] -> Just ((read x - 1, read y - 1), Vertical)
        [x,y,"v"] -> Just ((read x - 1, read y - 1), Vertical)
        _ -> Nothing
     
getMove :: [Flag] -> Maybe Move
getMove [] = Nothing
getMove (MoveFlag m:_) = readMove m 
getMove (_:flags) = getMove flags 

isMove :: Flag -> Bool
isMove (MoveFlag _) = True
isMove _ = False

getDepth :: [Flag] -> Maybe Int
getDepth [] = Just defaultDepth
getDepth (Depth d:_) = readMaybe d
getDepth (_:flags) = getDepth flags

main :: IO()
main = do
  args <- getArgs
  let (flags, inputs, errors) = getOpt Permute options args
  if (Help `elem` flags) || (not $ null errors)
  then putStrLn $ usageInfo (executableName ++ "[options] [filename]") options
  else 
    do let fName = if (null args)||(null inputs) 
                   then "initial.txt" 
                   else head inputs
       contents <- readFile fName
       case (readGame contents, getDepth flags) of
        --(Nothing, _) -> putStrLn "Invalid game file."
        (game, Nothing) -> putStrLn "Invalid depth flag."
        (game, Just depth) -> dispatch flags game depth

dispatch :: [Flag] -> Board -> Int -> IO()
dispatch flags game depth 
  | any isMove flags = moveIO flags game
  | Winner `elem` flags = showBestMove (Verbose `elem` flags) game
  | Interactive `elem` flags = interactiveIO depth game
  | otherwise = showGoodMove (Verbose `elem` flags) depth game

