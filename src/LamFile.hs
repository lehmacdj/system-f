{-# LANGUAGE OverloadedLists #-}

module LamFile where

import AST
import Parser

import Data.Map (Map)

import Control.Monad (join)
import Control.Lens

import Data.Semigroup

import Text.Parsec

import System.IO

data Declaration = Def Var Term
                 | Use FilePath
                 deriving (Show, Eq)

pDef :: Parser Declaration
pDef = Def <$> (Variable <$> identifier) <*> (lexeme (char '=') *> pTerm)

pUse :: Parser Declaration
pUse = Use <$> (lexeme (string "#use") *> stringLiteral)

pDeclaration :: Parser Declaration
pDeclaration = pDef <|> pUse

pDeclarations :: Parser [Declaration]
pDeclarations = many pDeclaration

readDeclaration :: String -> Either ParseError Declaration
readDeclaration = parse (whiteSpace *> pDeclaration <* eof) "Declaration"

readDeclarations :: String -> Either ParseError [Declaration]
readDeclarations = parse (whiteSpace *> pDeclarations <* eof) "Declarations"

getDeclarations :: FilePath -> IO [Declaration]
getDeclarations f = readFile f >>= pure
                                 . toListOf (_Right . traverse)
                                 . readDeclarations

normalizeDeclaration :: Declaration -> IO (Map Var Term)
normalizeDeclaration (Use f) = getDeclarations f >>= normalizeDeclarations
normalizeDeclaration (Def s t) = pure [(s, t)]

normalizeDeclarations :: [Declaration] -> IO (Map Var Term)
normalizeDeclarations ds = foldl (<>) [] <$> mapM normalizeDeclaration ds
