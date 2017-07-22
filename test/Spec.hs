{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import AST
import Evaluator
import Variables

main :: IO ()
main = defaultMain tests

tests = testGroup "Evaluator" [substituteTests]

substituteTests = testGroup "substitute"
    [ testCase "on Var succeeds" $
        substitute ("x" :: Var) (Var "y") (Var "x") @?= Var "y"
    , testCase "on Var fails" $
        substitute ("x" :: Var) (Var "y") (Var "z") @?= Var "z"
    , testCase "distributes over app" $
        substitute ("x" :: Var) (Var "z")
            (Var "x" `App` (Var "y" `App` Var "x"))
        @?= (Var "z" `App` (Var "y" `App` Var "z"))
    , testCase "avoids λ's" $
        substitute ("x" :: Var) (Var "y") (Lam "x" UnitTy (Var "x"))
        @?= Lam "x" UnitTy (Var "x")
    , testCase "doesn't avoid nonbinding λ's" $
        substitute ("z" :: Var) (Var "y") (Lam "x" UnitTy (Var "z"))
        @?= Lam "x" UnitTy (Var "y")
    , testCase "works on multiple variables" $
        substitute ("x" :: Var) (Var "y") (Var "x" `App` Var "x")
        @?= (Var "y" `App` Var "y")
    ]
