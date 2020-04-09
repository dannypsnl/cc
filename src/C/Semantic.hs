{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module C.Semantic (
  Context(..),
  CTypeDefinition(..),
  TypeID,
) where
import C.Semantic.Error
import Control.Monad.Trans
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text hiding (length)
import qualified Data.Text as T
import Numeric.Natural (Natural)

type Env = IORef Context

newType :: Env -> Text -> CTypeDefinition -> IOThrowsError ()
newType envRef typName typ = do
  Context{typeIDList, typeNameToID, variables} <- liftIO $ readIORef envRef
  liftIO $ writeIORef envRef (Context{typeIDList=typeIDList ++ [typ], typeNameToID=Map.insert typName (length typeIDList) typeNameToID, variables=variables})
  return ()

nullEnv :: IO Env
nullEnv = newIORef Context{typeIDList=[], typeNameToID=Map.empty, variables=Map.empty}

type TypeID = Int
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
