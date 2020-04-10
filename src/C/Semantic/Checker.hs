module C.Semantic.Checker (
  checkFile
) where
import C.Semantic
import C.Semantic.Error
import C.Syntax

checkFile :: Env -> [CDef] -> IO ()
checkFile env []            = return ()
checkFile env (cdef : rest) = do
  checkCDef env cdef
  checkFile env rest

checkCDef :: Env -> CDef -> IO ()
checkCDef env cdef = do
  case cdef of
    _ -> return $ ()
