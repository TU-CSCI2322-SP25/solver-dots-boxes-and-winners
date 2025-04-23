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