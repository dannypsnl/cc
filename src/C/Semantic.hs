{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module C.Semantic (
  nullEnv,
  newVar,
  lookupTypeID,
  newType,
  Env,
  Context(..),
  CTypeDefinition(..),
  SemiCType(..),
) where
import C.Semantic.Error
import Control.Monad.Trans
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text hiding (length)
import qualified Data.Text as T
import Numeric.Natural (Natural)
import Text.Megaparsec.Pos

type Env = IORef Context

newVar :: Env -> Text -> TypeID -> IO ()
newVar envRef varName typ = do
  env@Context{variables} <- liftIO $ readIORef envRef
  liftIO $ writeIORef envRef (env {variables=Map.insert varName typ variables})
  return ()

newType :: Env -> Text -> CTypeDefinition -> IO ()
newType envRef typName typ = do
  env@Context{typeIDList, typeNameToID} <- liftIO $ readIORef envRef
  liftIO $ writeIORef envRef (env {typeIDList=typeIDList ++ [typ], typeNameToID=Map.insert typName (length typeIDList) typeNameToID})
  return ()

lookupTypeID :: Env -> SourcePos -> Text -> IO TypeID
lookupTypeID envRef loc typName = do
  env@Context{typeNameToID, errors} <- liftIO $ readIORef envRef
  case Map.lookup typName typeNameToID of
    Nothing -> do
      -- record error
      liftIO $ writeIORef envRef (env { errors=errors ++ [ ReportError {
        errorLocation=loc
        , reportedError=NoTypeNamed typName} ] })
      return $ -1
    Just v  -> return v

data SemiCType = SemiTypeID TypeID
  | SemiArrow TypeID [TypeID]

type TypeID = Int
-- CTypeDefinition is not CType
-- the different is CType reference to CTypeDefinition for definition of a type
data CTypeDefinition = CBuiltinType Text
  | CStructType [(Text, TypeID)]

data Context = Context
  { typeIDList   :: [CTypeDefinition]
  , typeNameToID :: (Map Text TypeID)
  , variables    :: (Map Text TypeID)
  , errors       :: [ReportError]
  }

nullEnv :: IO Env
nullEnv = newIORef Context{
    typeIDList = []
    , typeNameToID = Map.empty
    , variables = Map.empty
    , errors = []}
