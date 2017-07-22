module Main where

import Test.Tasty
import Test.Tasty.HUnit

import AST
import Evaluator
import Variables

main :: IO ()
main = defaultMain tests

tests = testGroup "Evaluator" [unitTests]

unitTests = testGroup "Unit Tests"
    [ testCase "substitute var success" $
        substitute (Variable "x") (var "y") (var "x") @?= var "y"
    , testCase "substitute var fail" $
        substitute (Variable "x") (var "y") (var "z") @?= var "z"
    , testCase "substitute distributes over app" $
        substitute (Variable "x") (var "z")
            (var "x" `App` (var "y" `App` var "x"))
        @?= (var "z" `App` (var "y" `App` var "z"))
    , testCase "substitute avoids λ's" $
        substitute (Variable "x") (var "y") (Lam (Variable "x") UnitTy (var "x"))
        @?= Lam (Variable "x") UnitTy (var "x")
    ]
