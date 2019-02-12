module Lib (convertFiles, convertFile, convertPlantUMLToDot) where

import Parse
import Output
import Data.List (groupBy)

convertFiles :: [String] -> IO ()
convertFiles = mapM_ convertFile

convertFile :: String -> IO ()
convertFile fileName =
  readFile fileName >>= writeFile (toDot fileName) . convertPlantUMLToDot fileName

toDot :: String -> String
toDot = concat . (++ [".dot"]) . init . groupBy (const (/= '.'))

convertPlantUMLToDot :: String -> String -> String
convertPlantUMLToDot fileName = outputPlantUMLAsDot . unwrap . parsePlantUML fileName

unwrap (Left err) = error err
unwrap (Right x) = x
