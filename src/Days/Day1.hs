{-# LANGUAGE TemplateHaskell #-}
module Days.Day1
  ( day1
  ) where
import Days.Prelude
import Data.Maybe
import Linear.V2
import qualified Data.Map.Lazy as Map

data Dir = N | E | S | W deriving Enum

data Position = Position
  { _pos :: !(V2 Int)
  , _dir :: !Dir
  }

makeLenses ''Position

rotate :: Char -> Dir -> Dir
rotate 'R' W = N
rotate 'R' dir = succ dir
rotate 'L' N = W
rotate 'L' dir = pred dir

advance :: Position -> Position
advance p = p & pos +~ dp (p ^. dir)

dp :: Dir -> V2 Int
dp N = V2 0 1
dp S = V2 0 (-1)
dp E = V2 1 0
dp W = V2 (-1) 0

travelDistance :: V2 Int -> Int
travelDistance = sum . fmap abs

step :: [Position] -> (Char, Int) -> [Position]
step position (rotation, n) = take n . tail $ iterate advance p'
  where
    p' = last position & dir %~ rotate rotation

stops :: [(Char, Int)] -> [Position]
stops = concat . scanl' step [Position (V2 0 0) N]

part1 :: [(Char, Int)] -> Int
part1 = travelDistance . view pos . last . stops

part2 :: [(Char, Int)] -> Int
part2 =
  travelDistance .
  fromJust . snd . foldl' f (Map.empty, Nothing) . map (view pos) . stops
  where
    f (seen, x) p = (Map.insert p p seen, x <|> Map.lookup p seen)

day1 :: Day [(Char, Int)] Int Int
day1 =
  Day
  { _parser = parser
  , _dayPart1 = part1
  , _dayPart2 = part2
  }
  where
    parser :: Parser [(Char, Int)]
    parser = do
      let step = do
            rotation <- oneOf "RL"
            distance <- fromIntegral <$> natural
            pure (rotation, distance)
      step `sepBy1` string ", "
