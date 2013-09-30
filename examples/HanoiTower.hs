-- | The Tower of Hanoi is a game. There are 3 poles, or positions, where
--   differently-sized rings are placed. At the start, all the rings are on
--   the first pole, in order of size:
--   
--
-- >       _         _         _
-- >      | |       | |       | |
-- >      |_|       | |       | |
-- >     |___|      | |       | |
-- >    |_____|     | |       | |
-- >   |_______|    | |       | |
-- >  ===============================
--
--
--
--   The goal is to get all the rings to the last pole, following these rules:
--   - Only one piece may be moved at a time
--   - A piece may never be placed ontop of a smaller piece
--
--   To see it in action, you can type ```C-u 5 M-x hanoi``` in emacs

{-# LANGUAGE NoImplicitPrelude #-}
module HanoiTower where

import Language.Spaceship

-- the API is abstracted away, so that we can rewrite the internals
--    without any libraries that use it being affected

data Position = First | Middle | Last
   deriving (Show, Eq, Ord)

data Result = Solved
            | Legal Tower
            | Illegal

positionSize :: Tower -> Position -> Int
positionSize (Tower positions) position =
   length $ positions ! position

newTower :: Int -> Tower
newTower n =
   Tower $ gbTreeFromList [(First, [1..n]), (Middle, []), (Last, [])]

data Tower = Tower (GbTree Position [Int])

move :: Tower -> Position -> Position -> Result
move t Last Last = Legal t
move t from Last =
   case positionSize t from of
      0 -> Illegal
      1 -> if 0 == (positionSize t $ head $ [First, Middle] \\ [from])
             then Solved
             else moveMaybe t from Last
      _ -> moveMaybe t from Last
move t from to =
   if positionSize t from == 0
      then Illegal
      else moveMaybe t from to

-- | Makes the move -- assumes it's legal:
movePrime :: Tower -> Position -> Position -> Result
movePrime (Tower ps) from to =
   Legal $ Tower $
      insert to ((head (ps ! from)):(ps ! to)) (insert from (tail (ps ! from)) ps)

moveMaybe :: Tower -> Position -> Position -> Result
moveMaybe t@(Tower ps) from to =
   case headMaybe (ps ! to) of
      Nothing -> movePrime t from to
      Just n  ->
         if n <= (head (ps ! from))
            then movePrime t from to
            else Illegal

headMaybe []    = Nothing
headMaybe (x:_) = Just x
