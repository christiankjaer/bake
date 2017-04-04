module Parser
    ( BakeProgram
    , BakeItem(Build, Rule, Constant)
    , parser
    , parseCall
    , parseFromFile
    ) where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Maybe (isJust)

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

type BakeProgram = [BakeItem]

data BakeItem =
    Build { name :: String
          , targets :: [String]
          , dependencies :: [String]
          , forall :: Bool
          , commands :: [String]
          }
  | Rule { name :: String
         , variables :: [String]
         , body :: String
         }
  | Constant { name :: String
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

parseName :: Parser String
parseName = do
    init <- lowerChar
    rest <- many alphaNumChar
    return (init:rest)

parseHeader :: Parser (String, [String], [String], Bool)
parseHeader = do
    name <- parseName
    lexeme (char ':')
    targets <- sepEndBy parseFilename (many (oneOf " \t"))
    lexeme (string "<-")
    forall <- optional (char '<')
    skipMany (oneOf " \t")
    deps <- sepBy parseFilename (many (oneOf " \t"))
    return $ (name, targets, deps, isJust forall)

parseCommand :: Parser String
parseCommand = lexeme (some (noneOf "\n"))

parseBuild :: Parser BakeItem
parseBuild = L.nonIndented scn (L.indentBlock scn p)
    where
        p = do
            (name, targets, deps, forall) <- parseHeader
            return $ L.IndentMany Nothing (return . (Build name targets deps forall)) parseCommand

parseConstant :: Parser BakeItem
parseConstant = L.nonIndented scn p where
    p = do
        char '@'
        name <- parseName
        skipMany (oneOf " \t")
        lexeme (string "<-")
        skipMany (oneOf " \t")
        val <- some (noneOf "\n")
        return $ Constant name val

parseDecl :: Parser (String, [String])
parseDecl = do
    char '@'
    name <- parseName
    vars <- between (char '(') (char ')') $ sepBy1 parseName (lexeme $ char ',')
    return (name, vars)

parseCall :: Parser (String, [String])
parseCall = do
    char '@'
    name <- parseName
    vars <- between (char '(') (char ')') $ sepBy1 (some (noneOf ",()")) (lexeme $ char ',')
    return (name, vars)

parseRule :: Parser BakeItem
parseRule = L.nonIndented scn (L.indentBlock scn p)
    where
        p = do
            (name, vars) <- parseDecl
            return $ L.IndentMany Nothing (return . (\c -> Rule name vars (concat c))) parseCommand


parseItem :: Parser BakeItem
parseItem = try parseConstant <|> try parseRule <|> parseBuild

parser :: Parser BakeProgram
parser = do
    items <- many parseItem
    eof
    return items


parseFromFile p file = parse p file <$> readFile file
