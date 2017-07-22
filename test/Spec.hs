{-# LANGUAGE OverloadedStrings #-}

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
    [ testCase "substitute Var success" $
        substitute ("x" :: Var) (Var "y") (Var "x") @?= Var "y"
    , testCase "substitute Var fail" $
        substitute ("x" :: Var) (Var "y") (Var "z") @?= Var "z"
    , testCase "substitute distributes over app" $
        substitute ("x" :: Var) (Var "z")
            (Var "x" `App` (Var "y" `App` Var "x"))
        @?= (Var "z" `App` (Var "y" `App` Var "z"))
    , testCase "substitute avoids λ's" $
        substitute ("x" :: Var) (Var "y") (Lam (Variable "x") UnitTy (Var "x"))
        @?= Lam (Variable "x") UnitTy (Var "x")
    ]
