{- CIS 194 HW 11
   due Monday, 8 April
-}

module Cis194.Hw.SExpr where

import Cis194.Hw.AParser
import Control.Applicative
import Data.Char

type Ident = String

data Atom = N Integer
          | I Ident
  deriving Show

data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

oneOrMore :: Parser a -> Parser [a]
oneOrMore = some

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = many

spaces :: Parser String
spaces = zeroOrMore (char ' ')

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

parseSExpr :: Parser SExpr
parseSExpr = spaces *> ( A <$> atom <|> Comb <$> sexpr ) <* spaces where
  atom  = N <$> posInt <|> I <$> ident
  sexpr = char '(' *> oneOrMore parseSExpr <* char ')'
