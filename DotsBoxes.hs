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

data Winner = Win Player | None | Tie String deriving (Show)



dividePoints :: [Player] -> [Box] -> [(Player, Int)]
dividePoints [] bxs = []
dividePoints (pl:pls) bxs = (pl, length (filter (\(x, y) -> y==pl) bxs)):(dividePoints pls bxs)

winnerFromPoints :: [(Player, Int)] -> Winner
winnerFromPoints pts = if length trimmed == 1 then Win (head trimmed) else (Tie trimmed)
  where mst = maximum [y| (x,y) <- pts]
        trimmed = [x| (x,y) <- pts, y == mst]

findWinner :: Board -> Winner
findWinner bd = if length (boxes bd) < ((fst (size bd))) * ((snd (size bd))) then None else winnerFromPoints (dividePoints (show (order bd)) (boxes bd))

prettyPrintPlayers :: Board -> [Char] -> String
prettyPrintPlayers b [] = []
prettyPrintPlayers b (x:xs) = 
        let wins = length [ v | (u, v) <- (boxes b), v == x]
        in "Player " ++ [x] ++ " has " ++ show wins ++ " boxes \n" ++ (prettyPrintPlayers b xs)

prettyPrintBoard :: Board -> String
prettyPrintBoard b = 
        let num = fst (size b) * snd (size b)
            progress = length (boxes b)
            actions = length (grid b)
        in "The board is size: " ++ show num ++ "\n" ++
           show actions ++ " lines have been made \n" ++
           show progress ++ " boxes have been made in total \n" ++
           (prettyPrintPlayers b (order b))
