{-# LANGUAGE OverloadedStrings #-}
module C.ParserSpec where
import C.Parser
import C.Syntax
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import SpecHelper
import Test.Hspec.Megaparsec
import Text.Megaparsec hiding (State)

spec :: Spec
spec = describe "Parser" $ do
  context "variable definition" $ do
    it "pass" $ do
      tParse (parseVariableDef emptyContext) "int i;" `shouldParse` (CVariableDef "i" (TypeID 0))
    it "should failed" $ do
      tParse (parseVariableDef emptyContext) `shouldFailOn` ""
      -- missing name
      tParse (parseVariableDef emptyContext) `shouldFailOn` "int "
      -- missing `;`
      tParse (parseVariableDef emptyContext) `shouldFailOn` "int i"
  context "function definition" $ do
    it "pass" $ do
      tParse (parseFunctionDef emptyContext) "int add(int x, int y);" `shouldParse` (CFunctionDef "add" (CArrow (TypeID 0) [(TypeID 0),(TypeID 0)]) [("x", TypeID 0), ("y", TypeID 0)] Nothing)
    it "body" $ do
      tParse (parseFunctionDef emptyContext) "int add(int x, int y) {}" `shouldParse` (CFunctionDef "add" (CArrow (TypeID 0) [(TypeID 0),(TypeID 0)]) [("x", TypeID 0), ("y", TypeID 0)] (Just []))
    it "should failed" $ do
      tParse (parseFunctionDef emptyContext) `shouldFailOn` ""
      -- missing name
      tParse (parseFunctionDef emptyContext) `shouldFailOn` "int "
      -- missing parameters
      tParse (parseFunctionDef emptyContext) `shouldFailOn` "int add"
      -- missing body
      tParse (parseFunctionDef emptyContext) `shouldFailOn` "int add(int x, int y)"
  context "structure definition" $ do
    it "pass" $ do
      tParse (parseStructureDef emptyContext) "struct Car { int price; };" `shouldParse` (CStructureDef "Car" [("price", TypeID 0)])
    it "should failed" $ do
      tParse (parseStructureDef emptyContext) `shouldFailOn` ""
      -- missing name
      tParse (parseStructureDef emptyContext) `shouldFailOn` "struct"
      -- missing field part
      tParse (parseStructureDef emptyContext) `shouldFailOn` "struct Car"
      -- missing `}`
      tParse (parseStructureDef emptyContext) `shouldFailOn` "struct Car {;"
      -- missing `;`
      tParse (parseStructureDef emptyContext) `shouldFailOn` "struct Car {}"

tParse f code = parse f "" code

emptyContext :: ParseContext
emptyContext = ParseContext{
  typeIDList=[CBuiltinType (T.pack "int")]
  , typeNameToID=Map.fromList([(T.pack "int" , 0)])
  }
