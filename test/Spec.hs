import Test.Hspec

import qualified Day16Spec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Day16" Day16Spec.spec