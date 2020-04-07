module C.Syntax (
  CDef(..),
  TypeID,
  CType(..),
  CTypeDefinition(..),
  CStatement(..),
  CExpr(..),
) where
import Data.Text (Text)
import Numeric.Natural (Natural)

type TypeID = Natural
-- CType
-- TypeID: `0` -> CBuiltinType "void"
-- CArrow: `1(1, 1)`, `1` -> CBuiltinType "int"
-- CWrap: `2 1`, `2` -> CBuiltinType "*", `1` -> CBuiltinType "int"
data CType = TypeID TypeID
  | CArrow CType [CType]
  | CWrap CType CType
  deriving (Eq, Show)
-- CTypeDefinition is not CType
-- the different is CType reference to CTypeDefinition for definition of a type
data CTypeDefinition = CBuiltinType Text
  | CStructType

-- C
--
-- variable: int i;
-- function: int add(int, int);
-- structure:
--   struct Car {
--     char* name;
--     int price;
--   }
data CDef = CVariableDef Text CType
  | CFunctionDef Text CType [(Text, CType)] (Maybe [CStatement])
  | CStructureDef Text [(Text, CType)]
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
