module C.Syntax (
  CDef(..),
  CType(..),
  CStatement(..),
  CExpr(..),
) where
import C.Semantic
import Data.Text (Text)
import Text.Megaparsec.Pos

data CType = CTypeName Text
  deriving (Eq, Show)

-- C
--
-- variable: int i;
-- function: int add(int, int);
--   name return_type parameters body
-- structure:
--   struct Car {
--     char* name;
--     int price;
--   }
data CDef = CVariableDef Text CType
  | CFunctionDef Text CType [(Text, CType)] (Maybe [CStatement])
  | CStructureDef Text [(SourcePos, Text, CType)]
  deriving (Eq, Show)

-- CStatement
data CStatement = CLocalVar Text CType (Maybe CExpr)
  | CReturn (Maybe CExpr)
  deriving (Eq, Show)

-- CExpr
--
-- binary: 1 + 1, 0b1111 >> 1
-- string: ""
-- int: 0x0, 1
-- float: 1.43
data CExpr = CBinary CExpr CExpr
  | CString
  | CInt Integer
  | CFloat
  deriving (Eq, Show)
