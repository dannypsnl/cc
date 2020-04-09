module C.Semantic.Error (
  liftThrows,
  CError(..),
  ThrowsError,
  IOThrowsError,
) where
import Control.Monad.Except (ExceptT, throwError)
import Data.Text
import qualified Data.Text as T

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val
type ThrowsError = Either [CError]
type IOThrowsError = ExceptT [CError] IO

data CError = NoTypeNamed Text
  deriving (Eq, Show, Ord)
