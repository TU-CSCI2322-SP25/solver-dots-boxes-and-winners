module DotsBoxes where
import Data.List.Split (splitOn)  


type Point = (Int, Int)
type Player = Char
type Order = [Player]
type Box = (Point, Player)
data Dir = Vertical | Horizontal deriving (Show, Eq)
type Line = (Point, Dir)
type Move = Line
type Grid = [Line]
data Board = Board { size :: (Int, Int), grid :: Grid, boxes :: [Box], order :: Order}
data Winner = Win Player | Tie String deriving (Show, Eq)



showCycle (x:xs) = show (x:(takeWhile (/=x) xs))
instance Show Board where
  show (Board sz gr pt ord) = unwords ["Board", show sz, show gr, show pt, showCycle ord]

legalMoves :: Board -> [Move]
legalMoves bd = [x| x <- allMoves, not (x `elem` (grid bd))]
  where allMoves = [((x, y), d)| x <- [0.. (fst (size bd))], y <- [0.. (snd (size bd))], d <- [Vertical, Horizontal], not (d == Vertical && y == (snd (size bd))), not (d == Horizontal && x == (fst (size bd)))]

addBox :: Move -> Board -> Player -> [Box]
addBox ((x, y), d) b p = (boxes b) ++ vertL ++ vertR ++ horzT ++ horzB
        where vertL = if d == Vertical && ((x+1, y), Vertical) `elem` (grid b) && ((x, y), Horizontal) `elem` (grid b) && ((x,y+1), Horizontal) `elem` (grid b) then [((x, y), p)] else []
              vertR = if d == Vertical && ((x-1, y), Vertical) `elem` (grid b) && ((x-1, y), Horizontal) `elem` (grid b) && ((x-1,y+1), Horizontal) `elem` (grid b) then [((x-1, y), p)] else []
              horzT = if d == Horizontal && ((x, y+1), Horizontal) `elem` (grid b) && ((x, y), Vertical) `elem` (grid b) && ((x+1,y), Vertical) `elem` (grid b) then [((x, y), p)] else []
              horzB = if d == Horizontal && ((x, y-1), Horizontal) `elem` (grid b) && ((x, y-1), Vertical) `elem` (grid b) && ((x+1,y-1), Vertical) `elem` (grid b) then [((x, y-1), p)] else []

makeMove :: Board -> Move -> Maybe Board
makeMove b m = if fst (fst m) > fst (size b) || snd (fst m) > snd (size b) || m `elem` (grid b) then Nothing else 
        let nGrid = (grid b) ++ [m]
            nBoxes = addBox m b (head (order b))
            nOrder = if (length nBoxes) == (length (boxes b)) then tail (order b) else order b
        in Just $ b { grid = nGrid, boxes = nBoxes, order = nOrder} 
            

dividePoints :: [Player] -> [Box] -> [(Player, Int)]
dividePoints [] bxs = []
dividePoints (pl:pls) bxs = (pl, length (filter (\(x, y) -> y==pl) bxs)):(dividePoints pls bxs)

winnerFromPoints :: [(Player, Int)] -> Winner
winnerFromPoints pts = 
  case trimmed of 
    [x] -> Win x 
    _ -> Tie trimmed
  where mst = maximum [y| (x,y) <- pts]
        trimmed = [x| (x,y) <- pts, y == mst]

findWinner :: Board -> Maybe Winner
findWinner bd@(Board (xl, yl) _ bxs ord) = if length bxs < xl * yl 
                then Nothing 
                else Just $ winnerFromPoints (dividePoints (showCycle ord) bxs)


showGame :: Board -> String
showGame (Board sz gr bxs ord) =
    let sizeStr = "Size: " ++ show (fst sz) ++ "x" ++ show (snd sz)
        playersStr = "Players: " ++ ord
        gridStr = "Grid:\n" ++ unlines (map showLine gr)
        boxesStr = "Boxes:\n" ++ unlines (map showBox bxs)
    in unlines [sizeStr, playersStr, gridStr, boxesStr]

readGame :: String -> Board
readGame input = 
    let ls = lines input
        [width, height] = map read $ splitOn "," (head ls)
        players = ls !! 1          
        moves = concatMap parseMove (words (ls !! 2))
        boxes = parseBoxes (words (ls !! 3))
    in Board (width, height) moves boxes (cycle players)

parseMove :: String -> [Line]
parseMove s = 
    let parts = splitOn "," (filter (/=' ') s)
        [x,y] = map read (take 2 parts)
        dir = case last parts of
                "H" -> Horizontal
                "h" -> Horizontal
                "V" -> Vertical
                "v" -> Vertical
    in [((x,y), dir)]

parseBoxes :: [String] -> [Box]
parseBoxes [] = []
parseBoxes (s:ss) = 
    let parts = splitOn "," (filter (/=' ') s)
        [x,y] = map read (take 2 parts)
        player = last parts
    in ((x,y), head player) : parseBoxes ss

showLine :: Line -> String
showLine ((x, y), d) = "(" ++ show x ++ "," ++ show y ++ ") " ++ show d

showBox :: Box -> String
showBox ((x, y), p) = "(" ++ show x ++ "," ++ show y ++ ") " ++ [p]

prettyPrint board = unlines [ showIndex ind | ind <- [0..yl ]]
   where (xl, yl) = size board
         showIndex ind = showHorizontal 0 ind ++ "\n" ++ showVertical 0 ind
         showHorizontal s ind = 
                  if s < xl
                  then
                        if ((s, ind), Horizontal) `elem` (grid board)
                        then "._" ++ showHorizontal (s+1) ind
                        else ". " ++ showHorizontal (s+1) ind
                  else []
         -- print all the "._." or ". ." for the horizontal lines
         showVertical s ind = 
                  if s < xl
                  then
                        if ((s, ind), Vertical) `elem` (grid board)
                        then "| " ++ showVertical (s+1) ind
                        else "  " ++ showVertical (s+1) ind
                  else []
          -- print all the "|   |" or spaces for the vertical lines, unless the last index.

