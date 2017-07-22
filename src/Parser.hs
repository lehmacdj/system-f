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
    , P.opStart = oneOf "-@"
    , P.opLetter = oneOf "->@"
    , P.identStart = letter
    , P.identLetter = alphaNum <|> oneOf "()"
    , P.reservedNames = [ "forall", "unit", "()" ]
    , P.reservedOpNames = [ "->", "@" ]
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

data TyTerm = T Term
            | Ty Type

type TermTyApps = Maybe TyTerm

term :: Term -> TermTyApps
term = Just . T

ty :: Type -> TermTyApps
ty = Just . Ty

app :: TermTyApps -> TermTyApps -> TermTyApps
app s t = do
    T s' <- s
    T t' <- t
    Just $ T $ s' `App` t'

tyApp :: TermTyApps -> TermTyApps -> TermTyApps
tyApp s t = do
    T s' <- s
    Ty t' <- t
    Just $ T $ s' `TyApp` t'

binop :: String -> (a -> a -> a) -> Assoc -> Operator String u Identity a
binop name fn = Infix ((reservedOp name <|> reserved name) *> pure fn)

pVar :: Parser Term
pVar = Var . Variable <$> identifier

pLam :: Parser Term
pLam = Lam <$> (Variable <$> (lexeme (oneOf "λ\\") *> identifier))
           <*> (lexeme (char ':') *> pType)
           <*> (lexeme (char '.') *> pTerm)

pTyLam :: Parser Term
pTyLam = TyLam <$> (TyVariable <$> (lexeme (oneOf "%Λ") *> identifier))
               <*> (lexeme (char '.') *> pTerm)

pUnit :: Parser Term
pUnit = reserved "()" *> pure Unit

pTyTerm :: Parser TermTyApps
pTyTerm = buildExpressionParser
    [ [ Infix (reservedOp "@" *> pure tyApp) AssocLeft
      , Infix (whiteSpace *> pure app) AssocLeft
      ]
    ]
    pCompoundTyTerm

pCompoundTyTerm :: Parser TermTyApps
pCompoundTyTerm =
    (term <$> pUnit)
    <|> (term <$> pVar)
    <|> (term <$> pLam)
    <|> (ty <$> pType)
    <|> parens pTyTerm

pTerm :: Parser Term
pTerm = do
    Just (T t) <- pTyTerm
    pure t

pUnitTy :: Parser Type
pUnitTy = reserved "unit" *> pure UnitTy

pType :: Parser Type
pType = buildExpressionParser
    [ [ binop "->" FunTy AssocRight ] ]
    pCompoundType

pForall :: Parser Type
pForall = Forall <$> (TyVariable <$> (reserved "forall" *> char '\'' *> identifier))
                 <*> (lexeme (char '.') *> pType)

pTyVar :: Parser Type
pTyVar = TyVar . TyVariable <$> (char '\'' *> identifier)

pCompoundType :: Parser Type
pCompoundType = pUnitTy <|> pForall <|> pTyVar <|> parens pType

readTerm :: String -> Either ParseError Term
readTerm = parse (whiteSpace *> pTerm <* eof) "Term"

readType :: String -> Either ParseError Type
readType = parse (whiteSpace *> pType <* eof) "Type"

readTyVar :: String -> Either ParseError Type
readTyVar = parse (whiteSpace *> pTyVar <* eof) "TyVar"
