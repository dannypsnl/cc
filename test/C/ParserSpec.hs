{-# LANGUAGE OverloadedStrings #-}
module C.ParserSpec where
import C.Parser
import C.Semantic
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
      tParse parseVariableDef "int i;" `shouldParse` (CVariableDef (initialPos "") "i" "int")
    it "should failed" $ do
      tParse parseVariableDef `shouldFailOn` ""
      -- missing name
      tParse parseVariableDef `shouldFailOn` "int "
      -- missing `;`
      tParse parseVariableDef `shouldFailOn` "int i"
  context "function definition" $ do
    it "pass" $ do
      tParse parseFunctionDef "int add(int x, int y);" `shouldParse` (CFunctionDef "add" "int" [("x", "int"), ("y", "int")] Nothing)
    it "body" $ do
      tParse parseFunctionDef "int add(int x, int y) {}" `shouldParse` (CFunctionDef "add" "int" [("x", "int"), ("y", "int")] (Just []))
    it "should failed" $ do
      tParse parseFunctionDef `shouldFailOn` ""
      -- missing name
      tParse parseFunctionDef `shouldFailOn` "int "
      -- missing parameters
      tParse parseFunctionDef `shouldFailOn` "int add"
      -- missing body
      tParse parseFunctionDef `shouldFailOn` "int add(int x, int y)"
  context "statement" $ do
    it "local var" $ do
      tParse parseStmt "int i;" `shouldParse` (CLocalVar "i" "int" Nothing)
    it "return" $ do
      tParse parseStmt "return;" `shouldParse` (CReturn Nothing)
    it "return something" $ do
      tParse parseStmt "return 1;" `shouldParse` (CReturn (Just (CInt 1)))
  context "structure definition" $ do
    it "pass" $ do
      tParse parseStructureDef "struct Car { int price; };" `shouldParse` (CStructureDef "Car" [((SourcePos "" (mkPos 1) (mkPos 14)), "price", "int")])
    it "should failed" $ do
      tParse parseStructureDef `shouldFailOn` ""
      -- missing name
      tParse parseStructureDef `shouldFailOn` "struct"
      -- missing field part
      tParse parseStructureDef `shouldFailOn` "struct Car"
      -- missing `}`
      tParse parseStructureDef `shouldFailOn` "struct Car {;"
      -- missing `;`
      tParse parseStructureDef `shouldFailOn` "struct Car {}"

tParse f code = parse f "" code
