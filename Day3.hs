main =  print =<< solve paths. toSlope . map (concat . repeat) . lines <$> getContents

paths = 
  [(1,1)
  ,(3,1)
  ,(5,1)
  ,(7,1)
  ,(1,2)
  ]

solve xs slope = product $ map (`ride`  slope) xs

toSlope :: [String] -> Slope 
toSlope = map $ foldr (\x xs -> if x == '.' then Free : xs else Tree : xs) []

ride :: (Int, Int) -> Slope -> Int
ride _ [] = 0 
ride (r,d) slope@((x:_):_) =
  case x of 
    Tree -> 1 + ride (r,d) (map (drop r) (drop d slope))
    Free -> ride (r,d) $ map (drop r) (drop d slope)

type Slope = [[Point]]
data Point = Tree | Free deriving (Show)