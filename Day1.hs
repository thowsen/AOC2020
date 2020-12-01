import Data.List (sort)
main = print =<< solve 2020 . map read . words <$> getContents

-- O(n^2) non-exhaustive in case of no answer...
solve t xs = f sortedXs $ reverse sortedXs
  where sortedXs = sort xs
        f (z:zs) (y:ys)
          | z + y > t  = f (z:zs) ys
          | z + y <= t = 
            if (t-z-y) `elem` sortedXs 
              then (t-z-y)*z*y 
              else f zs (y:ys)  
          