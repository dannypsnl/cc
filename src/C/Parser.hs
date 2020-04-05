module C.Parser () where
import C.Syntax (CDef)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

--parseFile :: Parser [CDef]
--parseDef :: Parser CDef
parseVariableDef :: Parser CDef
parseVariableDef = parseType parseIdentifier

parseType :: Parser String
parseType = T.pack <$> some alphaNumChar

parseIdentifier :: Parser String
parseIdentifier = T.pack <$> some alphaNumChar
