module Main where

import Test.Tasty
import Test.Tasty.HUnit

import AST
import Evaluator

main :: IO ()
main = defaultMain tests

tests = testGroup "Evaluator" [unitTests]

unitTests = testGroup "Unit Tests"
    [ testCase "substitute var success" $
        substitute "x" (Var "y") (Var "x") @?= Var "y"
    , testCase "substitute var fail" $
        substitute "x" (Var "y") (Var "z") @?= Var "z"
    , testCase "substitute distributes over app" $
        substitute "x" (Var "z")
            (Var "x" `App` (Var "y" `App` Var "x"))
        @?= (Var "z" `App` (Var "y" `App` Var "z"))
    , testCase "substitute avoids λ's" $
        substitute "x" (Var "y") (Lam "x" UnitTy (Var "x"))
        @?= Lam "x" UnitTy (Var "x")
    ]
