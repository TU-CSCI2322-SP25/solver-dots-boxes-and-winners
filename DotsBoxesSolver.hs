module DotsBoxesSolver where
import DotsBoxes
import Data.List(intersect)
import Data.Maybe

isTieWith :: Player -> Winner -> Bool
isTieWith p (Win x) = False
isTieWith p (Tie xs) = p `elem` xs

whoWillWin :: Board -> Winner
whoWillWin (Board (sizeX, sizeY) gr bx ord)
    |length bx == sizeX * sizeY = fromJust (findWinner (Board (sizeX, sizeY) gr bx ord))
    |otherwise = 
        let paths = map whoWillWin (map (\x -> fromJust (makeMove (Board (sizeX, sizeY) gr bx ord) x)) (legalMoves (Board (sizeX, sizeY) gr bx ord)))
            ties = filter (isTieWith (head ord)) paths
        in  if (Win (head ord)) `elem` paths then Win (head ord) else if ties /= [] then head ties else head paths

bestMove :: Board -> Move 
bestMove bd = 
        let options = map (\x -> (x, whoWillWin (fromJust (makeMove (bd) x)))) (legalMoves (bd))
            wonGames = [m| (m,w) <- options, w == Win (head (order bd))]
            tiedGames = [m| (m,w) <- options, isTieWith (head (order bd)) w]
        in if wonGames /= [] then head wonGames else if tiedGames /= [] then head tiedGames else fst (head options)


rateGame :: Board -> Int 

rateGame bd
        |won == Just (Win (head (order bd))) = maxScore
        |won == Just (Win (head (tail (order bd)))) = -(maxScore)
        |otherwise = 
            let divided = dividePoints (showCycle (order bd)) (boxes bd)
            in snd (head divided) - snd (head (tail divided))
        where won = findWinner bd
              maxScore = fst (size bd) * snd (size bd)

specialMax :: (Int, Move) -> (Int, Move) -> (Int, Move)
specialMax (valA, mA) (valB, mB) = if valA > valB then (valA, mA) else (valB, mB)

specialMaximum :: [(Int, Move)] -> Int -> (Int, Move)
specialMaximum [] ma = (-ma, ((-1,-1),Horizontal))
specialMaximum (x:xs) ma = if fst x == ma then x else specialMax x (specialMaximum xs ma)

whoMightWin :: Int -> Board -> (Int, Move)

whoMightWin depth board
    |depth <= 0 = (rating,((-1,-1),Horizontal))
    |rating == maxi = (rating,((-1,-1),Horizontal))
    |rating == (-1 * maxi) = (rating,((-1,-1),Horizontal))
    |otherwise = specialMaximum [((-1) * (fst (whoMightWin (depth -1) (fromJust (makeMove board m)))), m)| m <- (legalMoves board)] maxi
        where rating = rateGame board
              maxi = fst (size board) * snd (size board)

