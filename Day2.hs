import Data.List.Split

main = print =<< length . filter valid2 . map (input2Instance . words) . lines <$> getContents

-- Part 1
valid1 :: Instance -> Bool
valid1 (Instance l u ch pw) = (\xs -> length xs <= u && length xs >= l) [x | x <- pw, x == ch]

input2Instance :: [String] -> Instance 
input2Instance [bounds, [ch,_], str] = Instance (read l) (read u) ch str
  where [l,u] =  "-" `splitOn` bounds

-- Part 2. horrible complexity due to linkedlist index.
valid2 :: Instance -> Bool
valid2 (Instance l u ch pw) = if pw !! (l-1) == ch then pw !! (u-1) /= ch else pw !! (u-1) == ch 

data Instance = Instance 
  {lowerbound :: Int
  ,upperbound :: Int
  ,letter     :: Char
  ,passwd     :: String
  } deriving (Show)