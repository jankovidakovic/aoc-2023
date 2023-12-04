module Day03 where
import Data.Vector (Vector, filter, null, fromList, (!?), toList, slice)
import Data.Char (isDigit)
import Paths_aoc2023 (getDataFileName)
import Data.Maybe (Maybe(Just), catMaybes)

data PartLocation = PartLocation {
  rowIndex :: Int,
  beginColumnIndex :: Int,
  endColumnIndex :: Int
}

maybeSlice :: Int -> Int -> Vector Char -> Maybe (Vector Char)
maybeSlice begin end vector = Just $ slice begin end vector

getAdjacentRow :: Int -> Int -> Int -> Vector (Vector Char) -> Vector Char
getAdjacentRow rowIndex beginColumnIndex endColumnIndex schematic = let 
  adjacentRow = schematic !? rowIndex
  adjacentLeftElem :: Maybe Char
  adjacentLeftElem = adjacentRow >>= (!? (beginColumnIndex-1))
  adjacentRightElem :: Maybe Char
  adjacentRightElem = adjacentRow >>= (!? (endColumnIndex+1))
  adjacentElems :: Maybe (Vector Char)
  adjacentElems = adjacentRow >>= maybeSlice beginColumnIndex endColumnIndex
  adjacentElemsList :: [Maybe Char]
  adjacentElemsList = map Just $ maybe [] toList adjacentElems
  maybes = catMaybes $ adjacentLeftElem : (adjacentElemsList ++ [adjacentRightElem])
  in fromList maybes

getAdjacentColumn :: Int -> Vector (Vector Char) -> Vector Char
getAdjacentColumn columnIndex = undefined
  

getFrame :: PartLocation -> Vector (Vector Char) -> Vector Char
getFrame PartLocation{rowIndex=ri, beginColumnIndex=cb, endColumnIndex=ce} schematic = let 
  topRow = getAdjacentRow (ri-1) cb ce
  botRow = getAdjacentRow (ri+1) cb ce
  leftCol = getAdjacentColumn 

  in undefined

getAdjacentElements :: PartLocation -> Vector (Vector Char) -> Vector Char
getAdjacentElements partLocation schematic = undefined

isSymbol :: Char -> Bool
isSymbol char = not (isDigit char) && char /= '.'

retainOnlySymbols :: Vector Char -> Vector Char
retainOnlySymbols = Data.Vector.filter isSymbol

isPartNumber :: PartLocation -> Vector (Vector Char) -> Bool
isPartNumber partLocation schematic = Data.Vector.null $ retainOnlySymbols $ getAdjacentElements partLocation schematic

parseInput :: [String] -> Vector (Vector Char)
parseInput = fromList . map fromList

day03 :: IO ()
day03 = do
    (getDataFileName "day03-input.txt" >>= readFile) >>= print . parseInput . lines
