module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Syntax

id_var :: Parser Var
id_var = (:) <$> upper <*> many (alphaNum <|> char '_')
     
id_atom :: Parser Atom
id_atom = (:) <$> lower <*> many (alphaNum <|> char '_')
--          <|> (:) <$> char '_' <*> many1 (alphaNum <|> char '_')

int :: Parser Int
int = read <$> (many1 digit)

term :: Parser Term
term =
  TComp <$> id_atom
        <*> (char '(' *> spaces *>
             sepBy1 (term <* spaces) (char ',' <* spaces)
             <* spaces <* char ')'
             <|> pure [])
  <|> TVar <$> id_var
  <|> TInt <$> int
  <|> char '_' *> return Wild
  <|> char '[' *> spaces *>
        (do hds <- sepBy1 (term <* spaces) $ char ',' <* spaces
            tl  <- char '|' *> spaces *> term <|> spaces *> return tNil
            return $ foldr tCons tl hds
        <|> spaces *> return tNil)
      <* char ']'

prop :: Parser Prop
prop = Prop <$> id_atom
            <*> (char '(' *> spaces *>
                sepBy1 (term <* spaces) (char ',' <* spaces) 
                <* spaces <* char ')'
                <|> pure [])

rule :: Parser Rule
rule =  
   Rule <$> prop <* spaces
        <*> (string ":-" *> spaces *>
             sepBy1 (prop <* spaces) (char ',' <* spaces)
             <|> pure []) <* char '.'

prog :: Parser Prog
prog = spaces *> many (rule <* spaces)

query :: Parser Query
query = spaces *> sepBy1 (prop <* spaces) (char ',' <* spaces) <* char '.'
