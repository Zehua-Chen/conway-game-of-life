import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

tests =
  [ testGroup
      "simulate"
      [ testCase
          ""
          ( do
              assertBool "" True
          )
      ]
  ]

main :: IO ()
main = defaultMain tests
