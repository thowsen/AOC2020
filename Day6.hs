import Data.List.Split (splitOn)
import Data.List (nub, intersect)

solve f = sum . map (length . f)

main = do 
  inp <- map lines . splitOn "\n\n" <$> getContents
  print $ (,) (solve (nub . concat) inp) (solve (foldr1 intersect) inp)

