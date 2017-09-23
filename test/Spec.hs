import Test.Hspec
import CompileAndRunSpec(compileAndRunSpec)
import EmitterSpec(emitterSpec)
import ParserSpec(parserSpec)

main :: IO ()
main = hspec $ do
  compileAndRunSpec
  emitterSpec
  parserSpec
