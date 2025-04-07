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

data Winner = Win Player | None | Tie String



dividePoints :: [Player] -> [Box] -> [(Player, [Box])]
dividePoints [] bxs = []
dividePoints (pl:pls) bxs = (pl, [x| x<-bxs, (snd x) == pl]):(dividePoints pls bxs)

winnerFromPoints :: [(Player, [Box])] -> Winner
winnerFromPoints pts = if length trimmed == 1 then Win (head trimmed) else (Tie trimmed)
  where mst = maximum [length y| (x,y) <- pts]
        trimmed = [x| (x,y) <- pts, length y == mst]

findWinner :: Board -> Winner
findWinner bd = if length (boxes bd) < ((fst (size bd)) - 1) * ((snd (size bd)) - 1) then None else winnerFromPoints (dividePoints (show (order bd)) (boxes bd))