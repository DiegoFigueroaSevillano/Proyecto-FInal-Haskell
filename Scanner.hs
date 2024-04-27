{-# LANGUAGE UndecidableInstances #-}

module Scanner where
import Data.Char (isAlphaNum)

-- cabal install uulib

type Col = Int

type Line = Int

type Value = String

type Input = String

data Token = Token Type Value Line Col

data Type
  = String
  | Link
  | URL
  | OpenBlock
  | EndBlock
  | Keyword
  | EndSlide
  | Error
  | Comment
  deriving (Eq, Ord)

instance Show Token where
  show (Token t v l c) = show t ++ show v ++ " " ++ show l ++ " " ++ show c ++ "\n"

instance Show Type where
  show String = "String: "
  show OpenBlock = "OpenBlock: "
  show EndBlock = "EndBlock: "
  show URL = "URL: "
  show Link = "Link: "
  show Keyword = "Keyword: "
  show Error = "Error: "
  show EndSlide = "EndSlide: "

instance (Eq Type) => (Eq Token) where
  (Token String s1 _ _) == (Token String s2 _ _) = True
  (Token OpenBlock _ _ _) == (Token OpenBlock _ _ _) = True
  (Token EndBlock _ _ _) == (Token EndBlock _ _ _) = True
  (Token URL _ _ _) == (Token URL _ _ _) = True
  (Token Link _ _ _) == (Token Link _ _ _) = True
  (Token Keyword k1 _ _) == (Token Keyword k2 _ _) = k1 == k2
  (Token Error _ _ _) == (Token Error _ _ _) = True
  (Token EndSlide _ _ _) == (Token EndSlide _ _ _) = True
  (Token t1 s1 _ _) == (Token t2 s2 _ _) = t1 == t2 && s1 == s2

instance Ord Token where
  compare x y | x == y = EQ
              | x <= y = LT
              | otherwise = GT
  (Token t1 s1 _ _) <= (Token t2 s2 _ _) = t1 < t2 || (t1 == t2 && s1 <= s2)


scanner :: Input -> [Token]
scanner xs = scan xs 1 1

scan :: Input -> Line -> Col -> [Token]
scan [] _ _ = []
scan (x:xs) l c
  | x == '!' = tokenizeTitle l c xs
  | x == '#' = tokenizeHeader [x] l c xs c
  | x == ' ' || x == '\n' = scan xs (updateLine x l) (updateCol x c)
  | x == ';' = scan (dropWhile (/= '\n') xs) (l + 1) 1
  | x == '{' = tokenizeOpenBlock x l c xs
  | x == '}' = tokenizeEndBlock x l c xs
  | x == '-' && head xs == '-' && head (tail xs) == '-' = tokenizeEndSlide l c xs
  | x == '^' = Token Keyword [x] l c : scan xs l (c+1)
  | x == '*' = Token Keyword [x] l c : scan xs l (c+1)
  | x == '<' = Token Keyword [x] l c : tokenizeLink l (c+1) xs "" 
  | x == '>' = Token Keyword [x] l c : scan xs l (c+1)
  | x == '&' = Token Keyword [x] l c : tokenizeImg l (c+1) xs ""
  | x == '$' = Token Keyword [x] l c : tokenizeImg l (c+1) xs ""
  | isAlphaNum x = tokenizeString x l c xs
  | otherwise = Token Error [x] l c : scan xs l (c + 1)

tokenizeLink :: Line -> Col -> Input -> String -> [Token]
tokenizeLink l c (x:xs) a = if x == ' ' then tokenizeImg l (c+1) xs a else if x /= '>' then tokenizeLink l c xs (a ++ [x]) else Token String a l c : scan (x:xs) (l+1) 1

tokenizeImg :: Line -> Col -> Input -> String -> [Token]
tokenizeImg l c (x:xs) a = if x == ' ' then tokenizeImg l (c+1) xs a else if x /= '\n' then tokenizeImg l c xs (a ++ [x]) else Token String a l c : scan xs (l+1) 1

tokenizeTitle :: Line -> Col -> Input -> [Token]
tokenizeTitle l c xs = Token Keyword "!" l c : scan xs l (c + 1) 

tokenizeHeader :: String -> Line -> Col -> Input -> Int -> [Token]
tokenizeHeader a l c (x:xs) b = if [x] == "#" then tokenizeHeader (a ++ [x]) l c xs (b+1) else Token Keyword a l c : scan xs l (b+2) 

tokenizeKeyword :: Char -> Line -> Col -> Input -> [Token]
tokenizeKeyword x l c xs = Token Keyword [x] l c : scan xs l (c + 1)

tokenizeOpenBlock :: Char -> Line -> Col -> Input -> [Token]
tokenizeOpenBlock x l c xs = Token OpenBlock [x] l c : scan xs l (c + 1)

tokenizeEndBlock :: Char -> Line -> Col -> Input -> [Token]
tokenizeEndBlock x l c xs = Token EndBlock [x] l c : scan xs l (c + 1)

tokenizeEndSlide :: Line -> Col -> Input -> [Token]
tokenizeEndSlide l c xs = Token EndSlide "---" l c : scan (drop 2 xs) l (c + 2)

tokenizeString :: Char -> Line -> Col -> Input -> [Token]
tokenizeString x l c xs =
  let (word, rest) = span isAlphaNumOrSpace xs
  in Token String (x:word) l c : scan rest l (c + length word + 1)
  where isAlphaNumOrSpace = (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ']))

updateLine :: Char -> Line -> Line
updateLine '\n' l = l + 1
updateLine _ l = l

updateCol :: Char -> Col -> Col
updateCol '\n' _ = 1
updateCol _ c = c + 1
