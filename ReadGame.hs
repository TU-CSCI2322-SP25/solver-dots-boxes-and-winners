module ReadGame where


import DotsBoxes

readGame :: String -> Board
readGame text =
  let ls = lines text
      sz = readSize (ls !! 0)
      ord = ls !! 1
      movesAndBoxes = words (unwords (drop 2 ls))
      (gridParts, boxParts) = splitAtGrid movesAndBoxes
      grid = map readLine gridParts
      boxes = map readBox boxParts
  in Board sz grid boxes ord

readSize :: String -> (Int, Int)
readSize line =
  let [a, b] = splitByComma line
  in (read a, read b)

readLine :: String -> Line
readLine str =
  let [x, y, d] = splitByComma str
      dir = if trim d == "H" then Horizontal else Vertical
  in ((read x, read y), dir)

readBox :: String -> Box
readBox str =
  let [x, y, p] = splitByComma str
  in ((read x, read y), head (trim p))

splitByComma :: String -> [String]
splitByComma s = case break (== ',') s of
  (a, ',' : rest) -> a : splitByComma rest
  (a, "")         -> [a]

splitAtGrid :: [String] -> ([String], [String])
splitAtGrid xs = span isLine xs
  where isLine s = last (trim s) == 'H' || last (trim s) == 'V'

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ')
