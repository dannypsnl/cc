{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module C.Parser (
  parseVariableDef,
  parseFunctionDef,
  parseStructureDef,
  ParseContext(..),
  Parser
) where
import C.Syntax
import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Map.Strict (Map)
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
data ParseContext = ParseContext
  { typeIDList   :: [CTypeDefinition]
  , typeNameToID :: (Map Text TypeID)
  }
type Parser = Parsec CustomError Text

--parseFile :: ParserSpec [CDef]
--parseDef :: ParserSpec CDef
parseVariableDef :: ParseContext -> Parser CDef
parseVariableDef env = do
  typ <- (parseType env)
  name <- parseIdentifier
  void (symbol ";")
  return $ CVariableDef name typ

parseFunctionDef :: ParseContext -> Parser CDef
parseFunctionDef env = do
  retTyp <- (parseType env)
  name <- parseIdentifier
  (params, pTypes) <- parens (parseParams env)
  void (symbol ";")
  return $ CFunctionDef name (CArrow retTyp pTypes) params

parseStructureDef :: ParseContext -> Parser CDef
parseStructureDef env = do
  void (symbol "struct")
  name <- parseIdentifier
  fields <- braces (many (parseStructureField env))
  void (symbol ";")
  return $ CStructureDef name fields

parseStructureField :: ParseContext -> Parser (Text, CType)
parseStructureField env = do
  typ <- parseType env
  name <- parseIdentifier
  void (symbol ";")
  return (name, typ)

parseParams :: ParseContext -> Parser ([(Text, CType)], [CType])
parseParams env = do
  fp <- parseParameter env
  restP <- (many ((symbol ",") >> (parseParameter env)))
  let params = (fp:restP)
  return (params, map (\(_, t) -> t) params)

parseParameter :: ParseContext -> Parser (Text, CType)
parseParameter env = do
  typ <- (parseType env)
  name <- parseIdentifier
  return (name, typ)

parseType :: ParseContext -> Parser CType
parseType ParseContext{typeNameToID} = do
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
symbol = L.symbol sc
sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)
