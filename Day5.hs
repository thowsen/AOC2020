import Data.List (sort)

rows = [0..127]
seats = [0..8]

solve :: [Int] -> [Int] -> String -> Int 
solve rows seats str = f rows (take 7 str) * 8 + f seats (drop 7 str)
  where 
    f xs [] = head xs
    f xs (s:str') = 
      case s of 
        'F' -> f (take (length xs `div` 2) xs) str'
        'B' -> f (drop (length xs `div` 2) xs) str'

main :: IO ()
main = let 
  repl 'L' = 'F'
  repl 'R' = 'B' 
  repl  x  =  x 
  in do 
    tickets <- sort . map (solve rows seats . map repl) . lines <$> readFile "./input.txt"
    print $ "Part 1 : " ++  show (last tickets)
    let seats = [(head tickets)..(last tickets)]
    print $ "Part 2 : " ++ show (head [x | x <- seats, x `notElem` tickets])
           