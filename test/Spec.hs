import Test.Hspec
import CompileAndRunSpec(compileAndRunSpec)

main :: IO ()
main = hspec $ do
  compileAndRunSpec
