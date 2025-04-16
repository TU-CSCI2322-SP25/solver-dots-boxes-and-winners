import DotsBoxes
import Data.List(intersect)
import Data.Maybe

whoWillWin :: Board -> Winner

whoWillWin (Board (sizeX, sizeY) gr bx ord)
    |length bx == sizeX * sizeY = fromJust (findWinner (Board (sizeX, sizeY) gr bx ord))
    |otherwise = 
        let paths = map whoWillWin (map (\x -> fromJust (makeMove (Board (sizeX, sizeY) gr bx ord) x)) (legalMoves (Board (sizeX, sizeY) gr bx ord)))
            isTieWithCur :: Winner -> Bool
            isTieWithCur (Win x) = False
            isTieWithCur (Tie xs) = (head ord) `elem` xs
            ties = filter isTieWithCur paths
        in  if (Win (head ord)) `elem` paths then Win (head ord) else if ties /= [] then head ties else head paths