{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module C.Parser () where
import C.Syntax (CDef (CVariableDef), TypeID)
import Control.Applicative hiding (some)
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

data Custom = NoTypeNamed Text
  deriving (Eq, Show, Ord)
instance ShowErrorComponent Custom where
  showErrorComponent (NoTypeNamed txt) = "no type named: " ++ T.unpack txt
noTypeNamed :: Text -> Parser a
noTypeNamed = customFailure . NoTypeNamed
data ParseContext = ParseContext
  { typeIDList   :: [Text]
  , typeNameToID :: (Map Text TypeID)
  }
type Parser = Parsec Custom Text

--parseFile :: Parser [CDef]
--parseDef :: Parser CDef
parseVariableDef :: ParseContext -> Parser CDef
parseVariableDef env = do
  varType <- (parseType env)
  varName <- parseIdentifier
  void (char ';')
  return CVariableDef{..}

parseType :: ParseContext -> Parser TypeID
parseType ParseContext{typeNameToID} = do
  s <- T.pack <$> some alphaNumChar
  case (Map.lookup s typeNameToID) of
    Nothing -> noTypeNamed s
    Just id -> return id

parseIdentifier :: Parser Text
parseIdentifier = do
  s <- T.pack <$> some alphaNumChar
  return s
