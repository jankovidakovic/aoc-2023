module Day02 where

import Data.Foldable (find)
import Data.List.Split (splitOn)
import Paths_aoc2023 (getDataFileName)

data Bag = Bag
    { red :: Int
    , green :: Int
    , blue :: Int
    }
    deriving (Show)

isColorPart :: String -> String -> Bool
isColorPart color part = take (length color) (reverse part) == reverse color

getNumOfColor :: String -> [String] -> Int
getNumOfColor color parts = case find (isColorPart color) parts of
    Nothing -> 0
    Just part -> read $ head $ splitOn " " part

readBag :: String -> Bag
readBag string =
    let
        parts = splitOn ", " string
        nRed = getNumOfColor "red" parts
        nGreen = getNumOfColor "green" parts
        nBlue = getNumOfColor "blue" parts
     in
        Bag{red = nRed, green = nGreen, blue = nBlue}

data Game = Game
    { gameId :: Int
    , turns :: [Bag]
    }

getMaxCubes :: (Bag -> Int) -> Game -> Int
getMaxCubes getNumCubes = maximum . map getNumCubes . turns

getMaxRed :: Game -> Int
getMaxRed = getMaxCubes red

getMaxBlue :: Game -> Int
getMaxBlue = getMaxCubes blue

getMaxGreen :: Game -> Int
getMaxGreen = getMaxCubes green

isGamePossible :: Int -> Int -> Int -> Game -> Bool
isGamePossible maxRed maxGreen maxBlue game = redOk && greenOk && blueOk
  where
    redOk = getMaxRed game <= maxRed
    greenOk = getMaxGreen game <= maxGreen
    blueOk = getMaxBlue game <= maxBlue

parseGame :: String -> Game
parseGame string =
    let
        gameAndRest = splitOn ": " string
        gameString = head gameAndRest
        parsedGameId = last $ splitOn " " gameString
        sTurns = splitOn "; " (last gameAndRest)
        bags = map readBag sTurns
     in
        Game{gameId = read parsedGameId, turns = bags}

solution1 :: [Game] -> Int
solution1 = sum . map gameId . filter (isGamePossible 12 13 14)

getPower :: Game -> Int
getPower game =
    let
        nRed = getMaxRed game
        nGreen = getMaxGreen game
        nBlue = getMaxBlue game
     in
        nRed * nGreen * nBlue

solution2 :: [Game] -> Int
solution2 = sum . map getPower

day02 :: IO ()
day02 = do
    (getDataFileName "day02-input.txt" >>= readFile) >>= print . solution2 . map parseGame . lines
