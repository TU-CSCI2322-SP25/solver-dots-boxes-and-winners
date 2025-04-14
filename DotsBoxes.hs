module DotsBoxes where

type Point = (Int, Int)
type Player = Char
type Order = [Player]
type Box = (Point, Player)
data Dir = Vertical | Horizontal deriving (Show, Eq)
type Line = (Point, Dir)
type Move = Line
type Grid = [Line]
data Board = Board { size :: (Int, Int), grid :: Grid, boxes :: [Box], order :: Order}
data Winner = Win Player | Tie String deriving (Show)


showCycle (x:xs) = show (x:(takeWhile (/=x) xs))
--instance Show Board where
  --show (Bd gr pt ord) = unword ["Board", show gr, show pt, showCycle ord]

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
            nOrder = tail (order b)
        in Just Board { size = (size b), grid = nGrid, boxes = nBoxes, order = if (length nBoxes) == (length (boxes b)) then nOrder else order b} 
            

dividePoints :: [Player] -> [Box] -> [(Player, Int)]
dividePoints [] bxs = []
dividePoints (pl:pls) bxs = (pl, length (filter (\(x, y) -> y==pl) bxs)):(dividePoints pls bxs)

winnerFromPoints :: [(Player, Int)] -> Winner
winnerFromPoints pts = if length trimmed == 1 then Win (head trimmed) else (Tie trimmed)
  where mst = maximum [y| (x,y) <- pts]
        trimmed = [x| (x,y) <- pts, y == mst]

findWinner :: Board -> Maybe Winner
findWinner bd = if length (boxes bd) < ((fst (size bd))) * ((snd (size bd))) then Nothing else Just winnerFromPoints (dividePoints (show (order bd)) (boxes bd))

prettyPrintPlayers :: Board -> [Char] -> String
prettyPrintPlayers b [] = []
prettyPrintPlayers b (x:xs) = 
        let wins = length [ v | (u, v) <- (boxes b), v == x]
        in "Player " ++ [x] ++ " has " ++ show wins ++ " boxes \n" ++ (prettyPrintPlayers b (tail (show (x:xs))))

prettyPrintBoard :: Board -> String
prettyPrintBoard b = 
        let num = fst (size b) * snd (size b)
            progress = length (boxes b)
            actions = length (grid b)
        in "The board is size: " ++ show num ++ "\n" ++
           show actions ++ " lines have been made \n" ++
           show progress ++ " boxes have been made in total \n" ++
           (prettyPrintPlayers b (order b))

