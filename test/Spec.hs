import Test.Hspec
import CompileAndRunSpec(compileAndRunSpec)
import EmitterSpec(emitterSpec)

main :: IO ()
main = hspec $ do
  compileAndRunSpec
  emitterSpec
