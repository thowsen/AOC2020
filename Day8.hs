{-# LANGUAGE LambdaCase #-}
import Data.Array (bounds, array, Array, (//), (!))
import Data.Char (isLetter, isDigit)
import Data.Maybe (fromMaybe, isJust)

main =  
  brute . 
  (\x -> array (0, length x) $ zip [0..] x) . 
  zip (repeat False) . 
  map read . 
  lines =<< readFile "input.txt"

-- Parsing boilerplate
data Instruction = Nop Op Int | Acc Op Int | Jmp Op Int deriving (Show)
instance Read Instruction where 
  readsPrec _ xs = case take 3 xs of 
    "nop" -> [(Nop op $ num xs, "")]
    "acc" -> [(Acc op $ num xs, "")]
    "jmp" -> [(Jmp op $ num xs, "")]
    where num = read . dropWhile (not . isDigit)
          op = (read :: String -> Op) $ 
            head ( dropWhile (\x -> isLetter x || x == ' ') xs) : ""

data Op = Sub | Add deriving Show
instance Read Op where
  readsPrec _ xs = case head xs of 
    '+' -> [(Add,"")] 
    '-' -> [(Sub,"")]

calc :: Op -> Int -> Int -> Int 
calc Add = (+)
calc Sub = (-)

try :: Array Int (Bool, Instruction) -> Int -> Int -> [Int] -> IO (Either Int [Int]) 
try code global pointer lastInstr
  | pointer >= snd (bounds code) = return $ Left global
  | otherwise                    = 
    case code ! pointer of 
      (True, _)           -> return $ Right lastInstr
      (_, s@(Jmp op num)) -> try (code // [(pointer,(True, s))]) global (calc op pointer num) (pointer:lastInstr)
      (_, s@(Acc op num)) -> try (code // [(pointer,(True, s))]) (calc op global num) (pointer + 1) lastInstr
      (_, s@(Nop _ _))    -> try (code // [(pointer,(True, s))]) global (pointer + 1) (pointer:lastInstr)

switch :: Int -> Array Int (Bool, Instruction) -> Array Int (Bool, Instruction)
switch index arr = case arr ! index of 
  (b, Nop op num) -> arr // [(index,(b, Jmp op num))] 
  (b, Jmp op num) -> arr // [(index,(b, Nop op num))]

-- CBA, list is not that big... exhaustive search on possible culprits.
brute :: Array Int (Bool, Instruction) -> IO Int
brute arr = try arr 0 0 [] >>= \case 
  Left val -> pure val
  Right xs -> head . (\xs -> [fromMaybe 0 x | x <- xs, isJust x]) <$> mapM (`trySwitch` arr) xs

trySwitch :: Int -> Array Int (Bool, Instruction) -> IO (Maybe Int)
trySwitch index arr = try (switch index arr ) 0 0 [] >>= \case
  Left _ -> pure $ return (index+1) 
  Right _ -> pure Nothing