module Main where

  import Test.Hspec

  -- unit under test
  import Parser (readExpr)
  import Value
  import LangError

  type SpecDef = (String, String, Value)

  specs :: [SpecDef]
  specs = [
      ("Number", "23", Number 24),
      ("String", "\"abc\"", String "abc")
    ]

  toSpec :: SpecDef -> SpecWith ()
  toSpec (name, input, output) = it name $
    extractValue (readExpr input) `shouldBe` output

  main :: IO ()
  main = hspec $ describe "Parsing" $ foldl1 (>>) $ map toSpec specs
