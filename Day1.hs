main = print =<< brute . map read . words <$> getContents

-- horrible complexity...
brute :: [Integer] -> Integer 
brute xs = head [x*y*z | x <- xs, y <- xs, z <- xs, x + y + z == 2020]