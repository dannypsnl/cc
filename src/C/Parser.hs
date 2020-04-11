{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module C.Parser (
  parseFile,
  parseVariableDef,
  parseFunctionDef,
  parseStructureDef,
  parseStmt,
  Parser
) where
import C.Semantic
import C.Semantic.Error
import C.Syntax
import Control.Applicative hiding (many, some)
import Control.Monad
import Control.Monad.Except
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ShowErrorComponent)
import Text.Megaparsec.Pos

instance ShowErrorComponent CError where
  showErrorComponent (NoTypeNamed txt) = "no type named: " ++ T.unpack txt

type Parser = Parsec Void Text

parseFile :: Parser [CDef]
parseFile = many parseDef

parseDef :: Parser CDef
parseDef = parseVariableDef
  <|> parseFunctionDef
  <|> parseStructureDef

parseVariableDef :: Parser CDef
parseVariableDef = do
  pos <- getSourcePos
  typ <- parseType
  name <- parseIdentifier
  void (symbol ";")
  return $ CVariableDef pos name typ

parseFunctionDef :: Parser CDef
parseFunctionDef = do
  retTyp <- (parseType)
  name <- parseIdentifier
  (params, pTypes) <- parens (parseParams)
  body <- parseFunctionBody
  return $ CFunctionDef name retTyp params body

parseFunctionBody :: Parser (Maybe [CStatement])
parseFunctionBody = (\_ -> Nothing) <$> void (symbol ";")
  <|> Just <$> braces (many (parseStmt))

parseStmt :: Parser CStatement
parseStmt = const <$> (parseStmt') <*> (void (symbol ";"))
parseStmt' :: Parser CStatement
parseStmt' = CReturn <$> ((void (keyword "return")) >> (option Nothing (Just <$> parseExpr)))
  <|> parseLocalVar

parseLocalVar :: Parser CStatement
parseLocalVar = do
  typ <- (parseType)
  name <- parseIdentifier
  return $ CLocalVar name typ Nothing

parseExpr :: Parser CExpr
parseExpr = CInt <$> integer

parseStructureDef :: Parser CDef
parseStructureDef = do
  void (keyword "struct")
  name <- parseIdentifier
  fields <- braces (many (parseStructureField))
  void (symbol ";")
  return $ CStructureDef name fields

parseStructureField :: Parser (SourcePos, Text, CType)
parseStructureField = do
  pos <- getSourcePos
  typ <- parseType
  name <- parseIdentifier
  void (symbol ";")
  return (pos, name, typ)

parseParams :: Parser ([(Text, CType)], [CType])
parseParams = do
  params <- (parseParameter) `sepBy` (symbol ",")
  return (params, map (\(_, t) -> t) params)

parseParameter :: Parser (Text, CType)
parseParameter = do
  typ <- (parseType)
  name <- parseIdentifier
  return (name, typ)

parseType :: Parser CType
parseType = parseIdentifier

parseIdentifier :: Parser Text
parseIdentifier = T.pack <$> lexeme (some alphaNumChar) <?> "identifier"

parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
symbol :: Text -> Parser Text
symbol w = L.symbol sc w <?> T.unpack w
keyword :: Text -> Parser Text
keyword w = L.symbol sc w <?> T.unpack w

integer :: Parser Integer
integer = L.signed sc (lexeme L.decimal)

-- sc
--
-- space consumer
sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)
