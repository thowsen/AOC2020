import Data.List.Split (splitOn)
import Data.List (nub, intersect)

solve f = print . sum . map f

main = do 
  inp <- splitOn "\n\n" <$> getContents
  solve (length . nub . concat . lines) inp 
  solve (length . (\(x:xs) -> foldr intersect x xs) . lines) inp