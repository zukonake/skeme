{-# LANGUAGE FlexibleInstances #-}
module Main where

  import Test.Hspec

  -- unit under test
  import Parser (readExpr)
  import Value
  import LangError
  import Eval

  class SpecLike a where
    toSpec :: a -> Spec

  type ParseSpecDef = (String, String, Value)
  type EvalSpecDef = (String, Value, Value)

  instance SpecLike [ParseSpecDef] where
    toSpec specdefs =
      foldl1 (>>) $ map parseToSpec specdefs
        where parseToSpec (name, input, output) =
                it name $ extractValue (readExpr input) `shouldBe` output

  instance SpecLike [EvalSpecDef] where
    toSpec specdefs =
      foldl1 (>>) $ map evalToSpec specdefs
        where evalToSpec (name, input, output) =
                it name $ extractValue (eval input) `shouldBe` output

  parseSpecs :: [ParseSpecDef]
  parseSpecs = [
      ("Number", "23", Number 23),
      ("String", "\"abc\"", String "abc"),
      ("Atom", "atm", Atom "atm"),
      ("Character", "#\\c", Character 'c'),
      ("Character.Space", "#\\space", Character ' '),
      ("Character.Newline", "#\\newline", Character '\n'),
      ("Boolean.True", "#t", Bool True),
      ("Boolean.False", "#f", Bool False),
      ("List", "(1 2 3)", List [Number 1, Number 2, Number 3]),
      ("Quoted List", "'(1 2 3)", List [Atom "quote", List [Number 1, Number 2, Number 3]]),
      ("Function Application", "(func 1 2 3)", List [Atom "func", Number 1, Number 2, Number 3])
    ]

  primitiveSpecs :: [EvalSpecDef]
  primitiveSpecs = [
      ("Plus",     List [Atom "+", Number 1, Number 2],          Number 3),
      ("Minus",    List [Atom "-", Number 1, Number 2],          Number (-1)),
      ("Times",    List [Atom "*", Number 3, Number 2],          Number 6),
      ("Divide",   List [Atom "/", Number 23, Number 3],         Number 7),
      ("Modulo",   List [Atom "mod", Number 22, Number 3],       Number 1),
      ("Quotient", List [Atom "quotient", Number 22, Number 3],  Number 7),
      ("Remainder",List [Atom "remainder", Number 22, Number 3], Number 1),

      ("<",  List [Atom "<",  Number 1, Number 2], Bool True),
      (">",  List [Atom ">",  Number 1, Number 2], Bool False),
      ("<=", List [Atom "<=", Number 1, Number 1], Bool True),
      (">=", List [Atom ">=", Number 1, Number 1], Bool True),
      ("&&", List [Atom "&&", Bool True, Bool False], Bool False),
      ("||", List [Atom "||", Bool True, Bool False], Bool True),

      ("car", List [Atom "car", List [Atom "quote", List [Number 1, Number 2, Number 3]]], Number 1),
      ("cdr", List [Atom "cdr", List [Atom "quote", List [Number 1, Number 2, Number 3]]], List [Number 2, Number 3]),

      ("If", List [Atom "if",
        List [Atom ">", Number 3, Number 2],
        List [Atom "+", Number 1, Number 2],
        List [Atom "*", Number 3, Number 5]], Number 3)
    ]

  predicateSpecs :: [EvalSpecDef]
  predicateSpecs = [
      ("Symbol predicate", List [Atom "symbol?", Atom "a"], Bool True),
      ("List predicate",   List [Atom "list?", List [Atom "quote", List []]], Bool True),
      ("Number predicate", List [Atom "number?", Number 1], Bool True),
      ("String predicate", List [Atom "string?", String "a"], Bool True),
      ("Char predicate",   List [Atom "character?", Character 'c'], Bool True),
      ("Bool predicate",   List [Atom "bool?", Bool True], Bool True),
      ("Symbol->String",   List [Atom "symbol->string", Atom "abc"], String "abc")
    ]

  main :: IO ()
  main = hspec $
    describe "Parse"     (toSpec parseSpecs)     >>
    describe "Primitive" (toSpec primitiveSpecs) >>
    describe "Predicate" (toSpec predicateSpecs)
