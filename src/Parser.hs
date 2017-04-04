module Parser
    ( BakeProgram
    , BakeItem(Build, Rule, Variable)
    , parser
    , parseCall
    , parseFromFile
    ) where

import Control.Applicative (empty)
import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

type BakeProgram = [BakeItem]

data BakeItem =
    Build { name :: String
          , targets :: [String]
          , dependencies :: [String]
          , commands :: [String]
          }
  | Rule { name :: String
         , variables :: [String]
         , body :: String
         }
  | Variable { name :: String
             , value :: String
             }
             deriving (Show)

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space (void spaceChar) lineComment empty

sc :: Parser ()
sc = L.space (void $ oneOf " \t") lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parseFilename :: Parser String
parseFilename = do
    s <- some (choice [alphaNumChar, oneOf ",.-_[]/"])
    return s

parseHeader :: Parser (String, [String], [String])
parseHeader = do
    name <- some lowerChar
    lexeme (char ':')
    targets <- sepEndBy parseFilename (many (oneOf " \t"))
    lexeme (string "<-")
    skipMany (oneOf " \t")
    deps <- sepBy parseFilename (many (oneOf " \t"))
    return $ (name, targets, deps)

parseCommand :: Parser String
parseCommand = lexeme (some (noneOf "\n"))

parseBuild :: Parser BakeItem
parseBuild = L.nonIndented scn (L.indentBlock scn p)
    where
        p = do
            (name, targets, deps) <- parseHeader
            return $ L.IndentMany Nothing (return . (Build name targets deps)) parseCommand

parseVariable :: Parser BakeItem
parseVariable = L.nonIndented scn p where
    p = do
        char '@'
        name <- some alphaNumChar
        skipMany (oneOf " \t")
        lexeme (string "<-")
        skipMany (oneOf " \t")
        val <- some (noneOf "\n")
        return $ Variable name val

parseDecl :: Parser (String, [String])
parseDecl = do
    char '@'
    name <- some lowerChar
    vars <- between (char '(') (char ')') $ sepBy1 (some lowerChar) (lexeme $ char ',')
    return (name, vars)

parseCall :: Parser (String, [String])
parseCall = do
    char '@'
    name <- some lowerChar
    vars <- between (char '(') (char ')') $ sepBy1 (some (noneOf ",()")) (lexeme $ char ',')
    return (name, vars)

parseRule :: Parser BakeItem
parseRule = L.nonIndented scn (L.indentBlock scn p)
    where
        p = do
            (name, vars) <- parseDecl
            return $ L.IndentMany Nothing (return . (\c -> Rule name vars (concat c))) parseCommand


parseItem :: Parser BakeItem
parseItem = try parseVariable <|> try parseRule <|> parseBuild

parser :: Parser BakeProgram
parser = do
    items <- many parseItem
    eof
    return items


parseFromFile p file = parse p file <$> readFile file
