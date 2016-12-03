{-# LANGUAGE TemplateHaskell #-}
module Days.Day1
  ( day1
  ) where
import Days.Prelude
import Data.Maybe
import qualified Data.Map.Lazy as Map

data Dir = N | E | S | W deriving (Enum, Eq, Show)

data Position = Position
  { _x :: Integer
  , _y :: Integer
  , _dir :: Dir
  } deriving (Show)

makeLenses ''Position

coords p = (p ^. x, p ^. y)

rotate 'R' W = N
rotate 'R' dir = succ dir
rotate 'L' N = W
rotate 'L' dir = pred dir

advance p = p & (x +~ dx d) & (y +~ dy d)
  where
    d = p ^. dir

dx N = 0
dx S = 0
dx E = 1
dx W = -1

dy N = 1
dy S = -1
dy E = 0
dy W = 0

travelDistance (x,y) = abs x + abs y

step :: [Position] -> (Char, Integer) -> [Position]
step position (rotation, n) = take (fromIntegral n) . tail $ iterate advance p'
  where
    p' = last position & dir %~ rotate rotation

stops = concat . scanl' step [Position 0 0 N]

part1 = travelDistance . coords . last . stops

part2 =
  travelDistance .
  fromJust . snd . foldl' f (Map.empty, Nothing) . map coords . stops
  where
    f (seen, x) p = (Map.insert p p seen, x <|> Map.lookup p seen)

day1 =
  Day
  { _parser = parser
  , _dayPart1 = part1
  , _dayPart2 = part2
  }
  where
    parser :: Parser [(Char, Integer)]
    parser = do
      let step = do
            rotation <- oneOf "RL"
            distance <- natural
            pure (rotation, distance)
      step `sepBy1` string ", "
