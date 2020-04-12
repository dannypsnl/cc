module C.Semantic.Checker (
  checkFile
) where
import C.Semantic
import C.Semantic.Error
import C.Syntax
import Data.Text
import Text.Megaparsec.Pos

checkFile :: Env -> [CDef] -> IO ()
checkFile env []            = return ()
checkFile env (cdef : rest) = do
  checkCDef env cdef
  checkFile env rest

checkCDef :: Env -> CDef -> IO ()
checkCDef env cdef = do
  case cdef of
    CVariableDef pos varName varTypName -> do
      typ <- lookupTypeID env pos varTypName
      newVar env varName typ
      return ()
    CStructureDef structName fields -> do
      typDef <- structDefinitionFromField env fields
      newType env structName typDef
      return ()
    _ -> return ()

structDefinitionFromField :: Env -> [(SourcePos, Text, CType)] -> IO CTypeDefinition
structDefinitionFromField env fields = do
  return $ CStructType []

fieldType :: Env -> (SourcePos, Text, CType) -> IO (Text, SemiCType)
fieldType env (pos, fieldName, typName) = do
  typID <- lookupTypeID env pos typName
  return $ (fieldName, SemiTypeID typID)
