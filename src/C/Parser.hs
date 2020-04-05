{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module C.Parser (
  parseVariableDef,
  ParseContext(..),
  Parser
) where
import C.Syntax (CDef (..), CType, TypeID)
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
  { typeIDList   :: [CType]
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
  return CVariableDef{varName=name, varType=typ}

parseType :: ParseContext -> Parser TypeID
parseType ParseContext{typeNameToID} = do
  s <- parseIdentifier
  case (Map.lookup s typeNameToID) of
    Nothing -> noTypeNamed s
    Just id -> return id

parseIdentifier :: Parser Text
parseIdentifier = T.pack <$> lexeme (some alphaNumChar)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
symbol :: Text -> Parser Text
symbol = L.symbol sc
sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)
