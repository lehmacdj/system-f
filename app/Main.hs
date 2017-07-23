{-# LANGUAGE TupleSections, OverloadedLists, NoImplicitPrelude #-}

module Main where

import Prelude hiding (getLine)
import Evaluator
import AST
import TyCheck
import Parser
import Variables

import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad.Free

import Control.Arrow
import Control.Monad.State
import Control.Monad.Trans.Class
import System.Console.Haskeline

import Text.Parsec.Char
import Text.Parsec

type Env = Map Var Term

type Repl = StateT Env (InputT IO)

replPrint :: String -> Repl ()
replPrint = lift . lift . putStrLn

putAndPrint :: (String, Maybe Env) -> Repl ()
putAndPrint (str, env) = do
    replPrint str
    maybe (pure ()) put env

data Command = Set String Term
             | Check Term
             | Evaluate Term
             | Parse Term
             | Quit
             deriving (Show)

pLine :: Parser Command
pLine =
    try (Set <$> identifier <*> (lexeme (char '=') *> pTerm))
    <|> try (Check <$> (lexeme (string ":t") *> pTerm))
    <|> try (lexeme (string ":q") *> pure Quit)
    <|> try (Parse <$> (lexeme (string ":p") *> pTerm))
    <|> (Evaluate <$> pTerm)

readCommand :: String -> Either String Command
readCommand s = left show $ parse (whiteSpace *> pLine <* eof) "Repl" s

evalCommand :: Command -> Repl ()
evalCommand Quit = pure ()
evalCommand (Set s t) = (M.insert (Variable s) t <$> get) >>= put
evalCommand (Check t) = replPrint (show $ typeCheck [] [] t)
evalCommand (Parse t) = replPrint (show t)
evalCommand (Evaluate t) = do
    sterm <- M.foldlWithKey' (\term k v -> substitute k v term) t <$> get
    case typeCheck [] [] sterm of
        Left err -> replPrint err
        Right _ -> case evaluate sterm of
           Left err -> replPrint err
           Right t' -> replPrint (show t')

repl :: Repl ()
repl = do
    line <- lift $ (readCommand <$>) <$> getInputLine "sysf> "
    case line of
      Nothing -> replPrint "Goodbye!" *> pure ()
      Just (Right Quit) -> pure ()
      Just (Right comm) -> evalCommand comm >> repl
      Just (Left err) -> replPrint err >> repl

main :: IO ()
main = runInputT defaultSettings $ fst <$> runStateT repl []
