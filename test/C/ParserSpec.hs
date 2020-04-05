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
      parse (parseVariableDef emptyContext) "" (T.pack "int i;") `shouldParse` (CVariableDef{varName=(T.pack "i"), varType=0})

emptyContext :: ParseContext
emptyContext = ParseContext{
  typeIDList=[CType (T.pack "int")]
  , typeNameToID=Map.fromList([(T.pack "int" , 0)])
  }
