{-
Unit testing and golden testing (unit testing based on files) for Spicy. Test all the difficult
parts of Spicy. This is especially Spicy.MolecularSystem and Spicy.Parser.
All tests are required to pass. There is no gray zone!!
-}
-- import           Data.Aeson
-- import           Data.Attoparsec.Text
-- import           RIO
-- import qualified RIO.Text                      as Text
-- import           Spicy.Class             hiding ( input )
-- import           Spicy.Generic
-- import           Spicy.Molecule
-- import           System.Path                    ( (</>) )
-- import qualified System.Path                   as Path
import           Test.Tasty
import Prelude
-- import           Test.Tasty.Golden

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" []
