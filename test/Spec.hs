import Test.Hspec
import CompileAndRunSpec(compileAndRunSpec)
import EmitterSpec(emitterSpec)
import ParserSpec(parserSpec)
import ParserWithPositionSpec(parserWithPositionSpec)

main :: IO ()
main = hspec $ do
  parserSpec
  parserWithPositionSpec
  emitterSpec
  compileAndRunSpec
  