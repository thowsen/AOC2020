import Data.Maybe (fromMaybe)

-- O(n)
main = do 
  inp <- map read . lines  <$> readFile "input.txt"
  let part1 = run 25 inp
  print $ "part 1: " ++ show part1
  print $ "part 2: " ++ show (fromMaybe (-1) $ part2 inp part1)
part2 lst ans = findSeries (takeWhile (< ans) lst) ans

run ::  Int -> [Int] -> Int
run preamble arr= if not (null hasSum) then run preamble (tail arr) else arr !! preamble
  where 
    hasSum = 
      filter (\(x,y) -> x+y == arr !! preamble) 
      [(x,y) | x <- take preamble arr, y <- take preamble arr, x /= y && x+y == arr !! preamble] 

-- O(n^2)
findSeries xs num = do 
  case run' 0 xs of 
    Nothing -> findSeries (tail xs) num 
    Just x -> Just $ minimum x + maximum x
  where 
    run' _ [] = Nothing 
    run' acc (x:xs) 
      | acc + x == num = Just [x]
      | acc + x > num = Nothing 
      | otherwise = (x:) <$> run' (acc+x) xs