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


legalMoves :: Board -> [Move]
legalMoves bd = [x| x <- allMoves, not (x `elem` (grid bd))]
  where allMoves = [((x, y), d)| x <- [0.. (fst (size bd))], y <- [0.. (snd (size bd))], d <- [Vertical, Horizontal], not (d == Vertical && y == (snd (size bd))), not (d == Horizontal && x == (fst (size bd)))]