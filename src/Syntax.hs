module Syntax (
  TypeID
) where

type TypeID = UInt

-- C
--
-- typedef
-- variable: int i;
-- function: int add(int, int);
-- structure: struct {}
-- union: union {}
data CDef = CTypeDef
  | CVariableDef
  | CFunctionDef
  | CStructureDef
  | CUnionDef

-- CExpr
--
-- binary: 1 + 1, 0b1111 >> 1
-- unary: -1
-- literal
data CExpr = CBinary
  | CUnary
  | CLiteral

-- CLiteral
--
-- string: ""
-- int: 0x0, 1
-- float: 1.43
data CLiteral = CString
  | CInt
  | CFloat
