module C.Semantic (
  Context(..),
  CTypeDefinition(..),
  TypeID,
) where
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text
import qualified Data.Text as T
import Numeric.Natural (Natural)

type TypeID = Natural
-- CTypeDefinition is not CType
-- the different is CType reference to CTypeDefinition for definition of a type
data CTypeDefinition = CBuiltinType Text
  | CStructType

data Context = Context
  { typeIDList   :: [CTypeDefinition]
  , typeNameToID :: (Map Text TypeID)
  , variables    :: (Map Text TypeID)
  }

emptyContext :: Context
emptyContext = Context {
  typeIDList=[CBuiltinType (T.pack "int")]
  , typeNameToID=Map.fromList([(T.pack "int" , 0)])
  , variables = Map.empty
  }
