import Data.List.Split
import Data.Array 
  ((!)
  , Array
  , elems
  , listArray
  )

main = print =<< length . filter valid2 . map (input2Instance . words) . lines <$> getContents

input2Instance [bounds, [ch,_], str] = Instance (read l) (read u) ch $ listArray (0, length str) str
  where [l,u] =  "-" `splitOn` bounds

-- Part 2. constant index access.
valid2 (Instance l u ch pw) = if pw ! (l-1) == ch then pw ! (u-1) /= ch else pw ! (u-1) == ch 

-- Part 1
valid1 (Instance l u ch pw) = (\xs -> length xs <= u && length xs >= l) [x | x <- elems pw, x == ch]

data Instance = Instance 
  {lowerbound :: Int
  ,upperbound :: Int
  ,letter     :: Char
  ,passwd     :: Array Int Char
  } deriving (Show)