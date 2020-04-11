module C.Semantic.Error (
  ReportError(..),
  CError(..),
) where
import Data.Text
import qualified Data.Text as T
import Text.Megaparsec.Pos

data ReportError = ReportError
  { errorLocation :: SourcePos
  , reportedError :: CError
  }

data CError = NoTypeNamed Text
  deriving (Eq, Show, Ord)
