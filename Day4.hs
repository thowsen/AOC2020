import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Char (isDigit)

data Passport = Passport 
  { byr :: Maybe String
  , iyr :: Maybe String
  , eyr :: Maybe String
  , hgt :: Maybe String
  , hcl :: Maybe String 
  , ecl :: Maybe String 
  , pid :: Maybe String
  , cid :: Maybe String
  } deriving Show

valid :: Passport -> Bool 
valid (Passport (Just byr) (Just iyr) (Just eyr) (Just hgt) (Just hcl) (Just ecl) (Just pid) cid) = 
  between byr 2002 1920  && 
  between iyr 2020 2010 && 
  between eyr 2030 2020 &&
  isHeight hgt && 
  isHex hcl && 
  isEyeColor ecl &&
  (length pid == 9 && all isDigit pid){- &&
  isJust cid  -} -- Missing CID is fine.
valid _ = False

main = print . length . filter valid . map (parse . sort . words) . splitOn "\n\n" =<< getContents

dummyPass = Passport Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 

parse :: [String] -> Passport
parse  = foldr (flip g . f) dummyPass 
  where
    f :: String -> (String, String)
    f = (\[a,b] -> (a, b)) . splitOn ":"
    g :: Passport -> (String, String) -> Passport
    g (Passport byr iyr eyr hgt hcl ecl pid cid) a = case a of
      ("byr", a) -> Passport (Just a) iyr eyr hgt hcl ecl pid cid
      ("iyr", a) -> Passport byr (Just a) eyr hgt hcl ecl pid cid
      ("eyr", a) -> Passport byr iyr (Just a) hgt hcl ecl pid cid
      ("hgt", a) -> Passport byr iyr eyr (Just a) hcl ecl pid cid
      ("hcl", a) -> Passport byr iyr eyr hgt (Just a) ecl pid cid
      ("ecl", a) -> Passport byr iyr eyr hgt hcl (Just a) pid cid
      ("pid", a) -> Passport byr iyr eyr hgt hcl ecl (Just a) cid
      ("cid", a) -> Passport byr iyr eyr hgt hcl ecl pid (Just a)
      x -> error $ show x

isHex ('#':xs) = all (\x -> isDigit x || x `elem` "abcdef") xs
isHex _ = False  

isEyeColor x = x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isHeight xs = 
    case metric of 
     "cm" -> digits >= 150 && digits <= 193
     "in" -> digits >= 59 && digits <= 76
     _    -> False
  where digits = (read :: String -> Int) $ takeWhile isDigit xs
        metric = dropWhile isDigit xs

between num upper lower = num' <= upper && num' >= lower
  where num' = (read :: String -> Int) num 