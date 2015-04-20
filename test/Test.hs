module Main where

--------------------------------------------------------------------------------
import           System.Log.CEF
import           Test.Tasty
import           Test.Tasty.HUnit
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMainWithIngredients ingredients tests
  where
    ingredients = [listingTests, antXMLRunner, consoleTestReporter]

