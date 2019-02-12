module Parse (parsePlantUML) where

import Text.Parsec
import Grammar
import Data.Char (isAlphaNum)
import Data.List (partition)
import Control.Monad.State

parsePlantUML :: String -> String -> Either String [Element]
parsePlantUML sourceName = either (Left . show) return . parse elements sourceName

elements = do
  elems <- sepBy element spaces
  eof
  return elems

element = try node <|> edge

node = do
  ntype <- nodeType
  hspaces1
  name <- className
  hspaces
  (fields, methods) <- (nodeBody <|> emptyBody)
  return $ Node ntype name fields methods
  where emptyBody = do
          hspaces
          endOfLineOrFile
          return ([], [])

nodeType = try (string "class" >> return Class) <|>
           try (string "interface" >> return Interface) <|>
           try (string "enum" >> return Enum) <|>
           (string "abstract" >> hspaces >> string "class" >> return AbstractClass)
           <?> "node type"

nodeBody = do
  char '{'
  endOfLine
  return . splitElements . reverse =<< nodeElements

splitElements :: [String] -> ([NodeElement], [NodeElement])
splitElements lines = (fields, methods)
  where (methodLines, fieldLines) = partition (elem '(') lines
        methods = fmap NodeElement methodLines
        fields = fmap NodeElement fieldLines

nodeElements = (char '}' >> endOfLineOrFile >> return []) <|> nextNodeElements
nextNodeElements = do
  x <- readLine
  xs <- nodeElements
  return (x:xs)

readLine = do
  line <- many (notFollowedBy endOfLine >> anyChar)
  endOfLine
  return line


edge = do
  leftName <- className
  hspaces
  leftHead <- option "" (string "<|" <|> string "<") <?> "left arrow head"
  style <- edgeStyle
  rightHead <- option "" (string "|>" <|> string ">") <?> "right arrow head"
  hspaces
  rightName <- className
  return $ Edge (EdgeEnd leftName leftHead) (EdgeEnd rightName rightHead) style ""

edgeStyle = (solid <|> dotted) <?> "arrow center"
  where solid = (string "--" <|> string "-") >> return Solid
        dotted = (string ".." <|> string ".") >> return Dotted

className = do
  x <- letter <?> "class name"
  xs <- many (satisfy (\c -> isAlphaNum c || c == '.'))
  return $ x:xs

hspace = (oneOf " \t")
hspaces = many hspace
hspaces1 = many1 hspace

endOfLineOrFile = (endOfLine >> return ()) <|> eof
