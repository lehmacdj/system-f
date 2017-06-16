module Parser where

import AST

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

import Control.Applicative (liftA2)
import Data.Functor.Identity

type Parser = Parsec String ()

tokenParser :: P.TokenParser u
tokenParser = P.makeTokenParser emptyDef
    { P.commentLine = "#"
    , P.opStart = oneOf "-"
    , P.opLetter = oneOf "->"
    , P.identStart = letter
    , P.identLetter = alphaNum <|> oneOf "()"
    , P.reservedNames = [ "unit", "()" ]
    , P.reservedOpNames = [ "->" ]
    , P.caseSensitive = True
    }

P.TokenParser
    { P.reservedOp = reservedOp
    , P.reserved = reserved
    , P.parens = parens
    , P.identifier = identifier
    , P.whiteSpace = whiteSpace
    , P.lexeme = lexeme
    } = tokenParser

binop :: String -> (a -> a -> a) -> Assoc -> Operator String u Identity a
binop name fn = Infix ((reservedOp name <|> reserved name) *> pure fn)

pVar :: Parser Term
pVar = Var <$> identifier

pLam :: Parser Term
pLam = Lam <$> (lexeme (oneOf "λ\\") *> identifier)
           <*> (lexeme (char ':') *> pType)
           <*> (lexeme (char '.') *> pTerm)

pUnit :: Parser Term
pUnit = reserved "()" *> pure Unit

pTerm :: Parser Term
pTerm =
    pUnit
    <|> pVar
    <|> pLam
    <|> (foldl1 App <$> parens (pTerm `sepEndBy` spaces))

pUnitTy :: Parser Type
pUnitTy = reserved "unit" *> pure UnitTy

pType :: Parser Type
pType = buildExpressionParser
    [ [ binop "->" FunTy AssocRight ] ]
    pCompoundType

pCompoundType :: Parser Type
pCompoundType = pUnitTy <|> parens pType

readTerm :: String -> Either ParseError Term
readTerm = parse (whiteSpace *> pTerm <* eof) "Term"

readType :: String -> Either ParseError Type
readType = parse (whiteSpace *> pType <* eof) "Type"
