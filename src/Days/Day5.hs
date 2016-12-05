module Days.Day5
  ( day5
  ) where

import Days.Prelude
import Crypto.Hash
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import Data.Char
import Data.List
import Data.Ord

packASCII :: String -> B.ByteString
packASCII = B.pack . map (fromIntegral . fromEnum)

numbers :: B.ByteString -> [B.ByteString]
numbers prefix =
  [ prefix `B.append` packASCII (show i)
  | i <- [0 ..] ]

hashes :: B.ByteString -> [String]
hashes prefix = map show hs
  where
    hs :: [Digest MD5]
    hs = map hash (numbers prefix)

isCodePart :: String -> Bool
isCodePart xs = all (== '0') $ take 5 xs



part1 :: B.ByteString -> String
part1 = map (!! 5) . take 8 . filter isCodePart . hashes

part2 :: B.ByteString -> String
part2 =
  map snd .
  sortBy (comparing fst) .
  Map.toList .
  head .
  filter (\m -> Map.size m == 8) . scanl' f Map.empty . filter isCodePart . hashes
  where
    f m x
      | d && ne = Map.insert loc char m
      | otherwise = m
      where
        (loc:char:_) = drop 5 x
        d = isOctDigit loc
        ne = not (Map.member loc m)

day5 =
  Day
  { _parser = parser
  , _dayPart1 = part1
  , _dayPart2 = part2
  }
  where
    parser :: Parser B.ByteString
    parser = packASCII <$> some alphaNum
