{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import AST
import Evaluator
import Variables
import TyCheck

main :: IO ()
main = defaultMain tests

tests = testGroup "tests" [substituteTests, typeCheckTests]

substituteTests = testGroup "substitute"
    [ substituteExpr
    , substituteType
    ]

substituteExpr = testGroup "Expr"
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

substituteType = testGroup "Type"
    [ testCase "on Var succeeds" $
        substitute ("x" :: TyVar) (TyVar "y") (TyVar "x") @?= TyVar "y"
    , testCase "on TyVar fails" $
        substitute ("x" :: TyVar) (TyVar "y") (TyVar "z") @?= TyVar "z"
    , testCase "distributes over app" $
        substitute ("x" :: TyVar) (TyVar "z")
            (TyVar "x" `FunTy` (TyVar "y" `FunTy` TyVar "x"))
        @?= (TyVar "z" `FunTy` (TyVar "y" `FunTy` TyVar "z"))
    , testCase "avoids λ's" $
        substitute ("x" :: TyVar) (TyVar "y") (Forall "x" (TyVar "x"))
        @?= Forall "x" (TyVar "x")
    , testCase "doesn't avoid nonbinding λ's" $
        substitute ("z" :: TyVar) (TyVar "y") (Forall "x" (TyVar "z"))
        @?= Forall "x" (TyVar "y")
    , testCase "works on multiple variables" $
        substitute ("x" :: TyVar) (TyVar "y") (TyVar "x" `FunTy` TyVar "x")
        @?= (TyVar "y" `FunTy` TyVar "y")
    ]

typeCheckTests = testGroup "typeCheck"
    [ testCase "base type" $ typeCheck [] [] Unit @?= Right UnitTy
    , testCase "var type in env" $
        typeCheck [("x", UnitTy)] [] (Var "x") @?= Right UnitTy
    , testCase "func type" $
        typeCheck [] [] (Lam "x" UnitTy Unit) @?= Right (FunTy UnitTy UnitTy)
    , testCase "func type extends env" $
        typeCheck [("x", FunTy UnitTy UnitTy)] [] (Lam "x" UnitTy Unit)
        @?= Right (FunTy UnitTy UnitTy)
    ]
