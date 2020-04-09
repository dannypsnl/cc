{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module C.Parser (
  parseVariableDef,
  parseFunctionDef,
  parseStructureDef,
  parseStmt,
  Parser
) where
import C.Semantic
import C.Syntax
import Control.Applicative hiding (many, some)
import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ShowErrorComponent)

data CustomError = NoTypeNamed Text
  deriving (Eq, Show, Ord)
instance ShowErrorComponent CustomError where
  showErrorComponent (NoTypeNamed txt) = "no type named: " ++ T.unpack txt
noTypeNamed :: Text -> Parser a
noTypeNamed = customFailure . NoTypeNamed
type Parser = Parsec CustomError Text

--parseFile :: ParserSpec [CDef]
--parseDef :: ParserSpec CDef
parseVariableDef :: Context -> Parser CDef
parseVariableDef env = do
  typ <- (parseType env)
  name <- parseIdentifier
  void (symbol ";")
  return $ CVariableDef name typ

parseFunctionDef :: Context -> Parser CDef
parseFunctionDef env = do
  retTyp <- (parseType env)
  name <- parseIdentifier
  (params, pTypes) <- parens (parseParams env)
  body <- parseFunctionBody env
  return $ CFunctionDef name (CArrow retTyp pTypes) params body

parseFunctionBody :: Context -> Parser (Maybe [CStatement])
parseFunctionBody env = (\_ -> Nothing) <$> void (symbol ";")
  <|> Just <$> braces (many (parseStmt env))

parseStmt :: Context -> Parser CStatement
parseStmt env = const <$> (parseStmt' env) <*> (void (symbol ";"))
parseStmt' :: Context -> Parser CStatement
parseStmt' env = CReturn <$> ((void (keyword "return")) >> (option Nothing (Just <$> parseExpr)))
  <|> parseLocalVar env

parseLocalVar :: Context -> Parser CStatement
parseLocalVar env = do
  typ <- (parseType env)
  name <- parseIdentifier
  return $ CLocalVar name typ Nothing

parseExpr :: Parser CExpr
parseExpr = CInt <$> integer

parseStructureDef :: Context -> Parser CDef
parseStructureDef env = do
  void (keyword "struct")
  name <- parseIdentifier
  fields <- braces (many (parseStructureField env))
  void (symbol ";")
  return $ CStructureDef name fields

parseStructureField :: Context -> Parser (Text, CType)
parseStructureField env = do
  typ <- parseType env
  name <- parseIdentifier
  void (symbol ";")
  return (name, typ)

parseParams :: Context -> Parser ([(Text, CType)], [CType])
parseParams env = do
  params <- (parseParameter env) `sepBy` (symbol ",")
  return (params, map (\(_, t) -> t) params)

parseParameter :: Context -> Parser (Text, CType)
parseParameter env = do
  typ <- (parseType env)
  name <- parseIdentifier
  return (name, typ)

parseType :: Context -> Parser CType
parseType Context{typeNameToID} = do
  s <- parseIdentifier
  case (Map.lookup s typeNameToID) of
    Nothing -> noTypeNamed s
    Just id -> return (TypeID id)

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
