module Main where
import Data.Maybe
import Text.Read
import System.IO
import System.Environment
import Control.Monad
import System.Console.GetOpt
import DotsBoxes

data Flag = Winner | Depth String | MoveFlag String | Verbose | Interactive | Help deriving (Show, Eq)

options :: [OptDescr Flag]
options = [ Option ['w'] ["winner"]        (NoArg Winner)           "Print the best move."
          , Option ['d'] ["depth"]         (ReqArg Depth "<num>")   "Specifies a cutoff depth, defaults to 4."
          , Option ['m'] ["move"]          (ReqArg MoveFlag "<move>") "Print the result of the move."
          , Option ['v'] ["verbose"]       (NoArg Verbose)          "Print both the move and result of the move."
          , Option ['i'] ["interactive"]   (NoArg Interactive)      "Play a game against the computer."
          , Option ['h'] ["help"]          (NoArg Help)             "Print usage information and exit."
          ]

executableName = "executableNameGoesHere"
defaultDepth = 3 --change these for your particular game.
--delete these four lines after you import your module!
-- to here

moveIO :: [Flag] -> Board -> IO()
moveIO flags game = 
  case getMove flags of
    Just movefl -> 
        case makeMove game movefl of
          Just g -> if Verbose `elem` flags 
                    then putStrLn (prettyPrint game) -- Put your prettyPrint here
                    else putStrLn (showGame game) -- put your showGame (in the file format) here 
          Nothing -> putStrLn "Error: illegal move"
    Nothing -> putStrLn "Error: invalid move format"

showGoodMove :: Bool  -> Int -> Board -> IO ()
showGoodMove False depth game = putStrLn "Feature not implemented" -- print the good move for the game
showGoodMove True depth game = putStrLn "Feature not implemented" -- print both a good move for the game, and who will win.

showBestMove :: Bool  -> Board -> IO ()
showBestMove False game = putStrLn "Feature not implemented" -- print the best move for the game
showBestMove True game = putStrLn "Feature not implemented" -- print both the best move for the game, and who will win.

interactiveIO :: Int -> Board -> IO ()
interactiveIO depth game = putStrLn "Feature not implemented" -- entirely optional interactive mode

readMove :: String -> Maybe Move
readMove = undefined -- convert a string into a move
     
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
        (Nothing, _) -> putStrLn "Invalid game file."
        (game, Nothing) -> putStrLn "Invalid depth flag."
        (Just game, Just depth) -> dispatch flags game depth

dispatch :: [Flag] -> Board -> Int -> IO()
dispatch flags game depth 
  | any isMove flags = moveIO flags game
  | Winner `elem` flags = showBestMove (Verbose `elem` flags) game
  | Interactive `elem` flags = interactiveIO depth game
  | otherwise = showGoodMove (Verbose `elem` flags) depth game

