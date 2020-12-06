import Data.List.Split (splitOn)
import Data.List (nub, intersect)

part1 = print . sum . map (length . nub . concat . lines)

part2 = print
  . sum . map (length . (\(x:xs) -> foldr intersect x xs) . lines)

main = do 
  inp <- splitOn "\n\n" <$> readFile "input.txt"
  part1 inp 
  part2 inp