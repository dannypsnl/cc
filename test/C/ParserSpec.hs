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
      parse (parseVariableDef emptyContext) "" "int i;" `shouldParse` (CVariableDef "i" (TypeID 0))

  context "function definition" $ do
    it "pass" $ do
      parse (parseFunctionDef emptyContext) "" "int add(int x, int y);" `shouldParse` (CFunctionDef "add" (CArrow (TypeID 0) [(TypeID 0),(TypeID 0)]) [])


emptyContext :: ParseContext
emptyContext = ParseContext{
  typeIDList=[CBuiltinType (T.pack "int")]
  , typeNameToID=Map.fromList([(T.pack "int" , 0)])
  }
