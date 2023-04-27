
module Parsing where

import Exp
import Lab2
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
    ( haskellStyle)
import Text.ParserCombinators.Parsec.Token

parseFirst :: Lab2.Parser a -> String -> Maybe a
parseFirst p s
  = case apply p s of
      [] -> Nothing
      (a,_):_ -> Just a

var :: Lab2.Parser Var
var = undefined
-- >>> parseFirst var "b is a var"
-- Just (Var {getVar = "b"})

varExp :: Lab2.Parser ComplexExp
varExp = undefined
-- >>> parseFirst varExp "b is a var"
-- Just (CX (Var {getVar = "b"}))

lambdaExp :: Lab2.Parser ComplexExp
lambdaExp = undefined
-- >>> parseFirst lambdaExp "\\x -> x"
-- Just (CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"})))

parenExp :: Lab2.Parser ComplexExp
parenExp = undefined
-- >>> parseFirst parenExp "(a)"
-- Just (CX (Var {getVar = "a"}))

basicExp :: Lab2.Parser ComplexExp
basicExp = undefined
-- >>> parseFirst basicExp "[a,b,c]"
-- Just (List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})])

expr :: Lab2.Parser ComplexExp
expr = varExp
-- >>> parseFirst expr "\\x -> x y z t"
-- Just (CLam (Var {getVar = "x"}) (CApp (CApp (CApp (CX (Var {getVar = "x"})) (CX (Var {getVar = "y"}))) (CX (Var {getVar = "z"}))) (CX (Var {getVar = "t"}))))

exprParser :: Lab2.Parser ComplexExp
exprParser = Lab2.whiteSpace *> expr <* endOfInput
-- >>> parseFirst exprParser "let x := 28 in \\y -> + x y"
-- Just (Let (Var {getVar = "x"}) (Nat 28) (CLam (Var {getVar = "y"}) (CApp (CApp (CX (Var {getVar = "+"})) (CX (Var {getVar = "x"}))) (CX (Var {getVar = "y"})))))

nanoHaskellDef :: LanguageDef st
nanoHaskellDef = haskellStyle
  { identStart = letter
  , identLetter = alphaNum Text.ParserCombinators.Parsec.<|> Text.ParserCombinators.Parsec.char '_'
  , reservedNames = [ "let", "in", "case", "of", "if", "then", "else"
                    , "True", "False", "not", "and", "or"
                    ]
  , reservedOpNames = [ "+", "-", "*", "/", "==", "/=", "<", ">", "<=", ">="
                      , "&&", "||", "!"
                      ]
  }

nanoHs :: TokenParser st
nanoHs = makeTokenParser nanoHaskellDef