module Main where

  import Test.Hspec

  -- unit under test
  import Parser (readExpr)
  import Value
  import LangError

  type SpecDef = (String, String, Value)

  specs :: [SpecDef]
  specs = [
      ("Number", "23", Number 23),
      ("String", "\"abc\"", String "abc"),
      ("Atom", "atm", Atom "atm"),
      ("List", "(1 2 3)", List [Number 1, Number 2, Number 3]),
      ("Quoted List", "'(1 2 3)", List [Atom "quote", List [Number 1, Number 2, Number 3]]),
      ("Character", "#\\c", Character 'c'),
      ("Character.Space", "#\\space", Character ' '),
      ("Character.Newline", "#\\newline", Character '\n'),
      ("Boolean.True", "#t", Bool True),
      ("Boolean.False", "#f", Bool False)
    ]

  toSpec :: SpecDef -> SpecWith ()
  toSpec (name, input, output) = it name $
    extractValue (readExpr input) `shouldBe` output

  main :: IO ()
  main = hspec $ describe "Parsing" $ foldl1 (>>) $ map toSpec specs
