{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Days.Day2
  ( day2
  ) where

import Days.Prelude
import Data.Array.IArray


data Act = U | R | D | L | P

data Pos = Pos
  { _x :: Int
  , _y :: Int
  , _code :: [Char]
  }

makeLenses ''Pos

type Pad = Array (Int, Int) Char

keyPad1 :: Pad
keyPad1 = listArray ((0, 0), (4, 4))
  "     \
  \ 123 \
  \ 456 \
  \ 789 \
  \     "

keyPad2 :: Pad
keyPad2 = listArray ((0, 0), (6, 6))
  "       \
  \   1   \
  \  234  \
  \ 56789 \
  \  ABC  \
  \   D   \
  \       "

digitAt m p = m ! (p ^. y, p ^. x)

canMoveTo m p = digitAt m p /= ' '

move m f p
  | canMoveTo m (f p) = f p
  | otherwise = p

press m p = p & code %~ (++ [digitAt m p])

act m U = move m (y -~ 1)
act m D = move m (y +~ 1)
act m L = move m (x -~ 1)
act m R = move m (x +~ 1)
act m P = press m

solve m = view code . foldl' (flip (act m)) (Pos x y [])
  where
    (y,x) = fst . head . filter ((== '5') . snd) $ assocs m

part1 = solve keyPad1

part2 = solve keyPad2

day2 =
  Day
  { _parser = parser
  , _dayPart1 = part1
  , _dayPart2 = part2
  }
  where
    parser :: Parser [Act]
    parser =
      some $
      choice
        [ char 'U' >> pure U
        , char 'R' >> pure R
        , char 'D' >> pure D
        , char 'L' >> pure L
        , char '\n' >> pure P
        ]
