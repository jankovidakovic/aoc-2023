module Day01 where

import Data.Char
import Paths_aoc2023 (getDataFileName)

getCalibrationValue :: String -> Int
getCalibrationValue row =
    let firstDigit = getFirstDigit row
        lastDigit = getLastDigit row
     in 10 * firstDigit + lastDigit

getFirstDigit :: String -> Int
getFirstDigit string =
    let
        firstDigitWord = findFirstMatch allDigitWords string
        firstDigit = findFirstMatch allDigits string
     in
        resolveTwoMatchesOn (<) firstDigitWord firstDigit

getLastDigit :: String -> Int
getLastDigit string =
    let
        lastDigitWord = findLastMatch allDigitWords string
        lastDigit = findLastMatch allDigits string
     in
        resolveTwoMatchesOn (>) lastDigitWord lastDigit

resolveTwoMatchesOn :: (Int -> Int -> Bool) -> Maybe (String, Int) -> Maybe (String, Int) -> Int
resolveTwoMatchesOn _ Nothing Nothing = error "no valid match"
resolveTwoMatchesOn _ Nothing (Just (match, _)) = getDigitValue match
resolveTwoMatchesOn _ (Just (match, _)) Nothing = getDigitValue match
resolveTwoMatchesOn op (Just (firstMatch, i)) (Just (secondMatch, j)) =
    let match = if op i j then firstMatch else secondMatch
     in getDigitValue match

findFirstMatch :: [String] -> String -> Maybe (String, Int)
findFirstMatch candidates string = go candidates string 0
  where
    go _ "" _ = Nothing
    go candidates string i = case getPrefixMatch string candidates of
        Just match -> Just (match, i)
        Nothing -> go candidates (tail string) (i + 1)

findLastMatch :: [String] -> String -> Maybe (String, Int)
findLastMatch candidates string = case findFirstMatch (map reverse candidates) (reverse string) of
    Nothing -> Nothing
    Just (match, i) -> Just (reverse match, length string - length match - i)

getSimpleCalibrationValue :: String -> Int
getSimpleCalibrationValue string = read [firstDigit, lastDigit]
  where
    digits = filter isDigit string
    firstDigit = head digits
    lastDigit = last digits

solution :: [String] -> Int
solution = sum . map getSimpleCalibrationValue

isPrefix :: String -> String -> Bool
isPrefix string prefix = prefix == take (length prefix) string

getPrefixMatch :: String -> [String] -> Maybe String
getPrefixMatch string = safeHead . filter (isPrefix string)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

getDigitValue :: String -> Int
getDigitValue "nine" = 9
getDigitValue "eight" = 8
getDigitValue "seven" = 7
getDigitValue "six" = 6
getDigitValue "five" = 5
getDigitValue "four" = 4
getDigitValue "three" = 3
getDigitValue "two" = 2
getDigitValue "one" = 1
getDigitValue "9" = 9
getDigitValue "8" = 8
getDigitValue "7" = 7
getDigitValue "6" = 6
getDigitValue "5" = 5
getDigitValue "4" = 4
getDigitValue "3" = 3
getDigitValue "2" = 2
getDigitValue "1" = 1
getDigitValue _ = error "no value"

allDigitWords :: [String]
allDigitWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

allDigits :: [String]
allDigits = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

solution2 :: [String] -> Int
solution2 = sum . map getCalibrationValue

day01 :: IO ()
day01 = do
    (getDataFileName "day01-input.txt" >>= readFile) >>= print . solution2 . lines
