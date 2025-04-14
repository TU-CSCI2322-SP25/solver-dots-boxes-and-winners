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

showCycle (x:xs) = show (x:(takeWhile (/=x) xs))
--instance Show Board where
  --show (Bd gr pt ord) = unword ["Board", show gr, show pt, showCycle ord]

data Winner = Player | None | Tie String

addBox :: Move -> Board -> Player -> [Box]
addBox ((x, y), d) b p = (boxes b) ++ vertL ++ vertR ++ horzT ++ horzB
        where vertL = if d == Vertical && ((x+1, y), Vertical) `elem` (grid b) && ((x, y), Horizontal) `elem` (grid b) && ((x,y+1), Horizontal) `elem` (grid b) then [((x, y), p)] else []
              vertR = if d == Vertical && ((x-1, y), Vertical) `elem` (grid b) && ((x-1, y), Horizontal) `elem` (grid b) && ((x-1,y+1), Horizontal) `elem` (grid b) then [((x-1, y), p)] else []
              horzT = if d == Horizontal && ((x, y+1), Horizontal) `elem` (grid b) && ((x, y), Vertical) `elem` (grid b) && ((x+1,y), Vertical) `elem` (grid b) then [((x, y), p)] else []
              horzB = if d == Horizontal && ((x, y-1), Horizontal) `elem` (grid b) && ((x, y-1), Vertical) `elem` (grid b) && ((x+1,y-1), Vertical) `elem` (grid b) then [((x, y-1), p)] else []

makeMove :: Board -> Move -> Board
makeMove b m =
        let nGrid = (grid b) ++ [m]
            nBoxes = addBox m b (head (order b))
        in Board { size = (size b), grid = nGrid, boxes = nBoxes, order = (order b)} 
            
