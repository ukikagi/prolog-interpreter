module Parser where

import Data.Functor (($>))
import Syntax
  ( Atom,
    Prog,
    Prop (..),
    Query,
    Rule (..),
    Term (..),
    Var,
    tCons,
    tNil,
  )
import Text.Parsec
  ( alphaNum,
    char,
    digit,
    lower,
    many,
    many1,
    sepBy1,
    spaces,
    string,
    upper,
    (<|>),
  )
import Text.Parsec.String (Parser)

idVar :: Parser Var
idVar = (:) <$> upper <*> many (alphaNum <|> char '_')

idAtom :: Parser Atom
idAtom = (:) <$> lower <*> many (alphaNum <|> char '_')

--          <|> (:) <$> char '_' <*> many1 (alphaNum <|> char '_')

int :: Parser Int
int = read <$> many1 digit

term :: Parser Term
term =
  TComp <$> idAtom
    <*> ( char '(' *> spaces
            *> sepBy1 (term <* spaces) (char ',' <* spaces)
            <* spaces
            <* char ')'
            <|> pure []
        )
    <|> TVar <$> idVar
    <|> TInt <$> int
    <|> char '_' Data.Functor.$> Wild
    <|> char '[' *> spaces
      *> ( do
             hds <- sepBy1 (term <* spaces) $ char ',' <* spaces
             tl <- char '|' *> spaces *> term <|> spaces Data.Functor.$> tNil
             return $ foldr tCons tl hds
             <|> spaces Data.Functor.$> tNil
         )
      <* char ']'

prop :: Parser Prop
prop =
  Prop <$> idAtom
    <*> ( char '(' *> spaces
            *> sepBy1 (term <* spaces) (char ',' <* spaces)
            <* spaces
            <* char ')'
            <|> pure []
        )

rule :: Parser Rule
rule =
  Rule <$> prop <* spaces
    <*> ( string ":-" *> spaces
            *> sepBy1 (prop <* spaces) (char ',' <* spaces)
            <|> pure []
        )
      <* char '.'

prog :: Parser Prog
prog = spaces *> many (rule <* spaces)

query :: Parser Query
query = spaces *> sepBy1 (prop <* spaces) (char ',' <* spaces) <* char '.'
