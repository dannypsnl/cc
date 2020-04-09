module C.Semantic (
  Context(..),
  CTypeDefinition(..),
  TypeID,
) where
import Data.Map.Strict (Map)
import Data.Text
import Numeric.Natural (Natural)

type TypeID = Natural
-- CTypeDefinition is not CType
-- the different is CType reference to CTypeDefinition for definition of a type
data CTypeDefinition = CBuiltinType Text
  | CStructType

data Context = Context
  { typeIDList   :: [CTypeDefinition]
  , typeNameToID :: (Map Text TypeID)
  }
