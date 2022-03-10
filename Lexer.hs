module Lexer where
import Data.Char (isUpper, isLower, isAlpha, isDigit, isSpace)
import Token (Token (Token), TokenType (COLUMN, QMARK, STAR, BAR, PLUS, SEMI, Terminal, NonTerminal, Literal, LPAREN, RPAREN))
import Text.Read.Lex (isSymbolChar)
import Data.List (isPrefixOf)

isTerminalHead :: Char -> Bool
isTerminalHead = isUpper

isTerminalChar :: Char -> Bool
isTerminalChar c = isUpper c || c == '_'

isNonTerminalHead :: Char -> Bool
isNonTerminalHead = isLower

isNonTerminalChar :: Char -> Bool
isNonTerminalChar c = isAlpha c || isDigit c || c == '_'

split :: Char -> String -> [String]
split c xs = case break (==c) xs of 
  (ls, "") -> [ls]
  (ls, x:rs) -> ls : split c rs

matchUntil :: (String -> Bool) -> String -> (String, String)
matchUntil _ [] = ([], [])
matchUntil f b@(x:xs) = if f b
                 then ([], b)
                 else let (a', b') = matchUntil f xs
                      in (x:a', b')

parseComment :: String -> (String, String)
parseComment = matchUntil (isPrefixOf "\n")

parseMultiComment :: String -> (String, String)
parseMultiComment = matchUntil (isPrefixOf "*/")

tokenize :: String -> Int -> Int -> [Token]
                   -- current row number
                          -- current column number
tokenize [] _ _ = []

tokenize ('/' : '*' : xs) r p = tokenize (drop 2 rem) rx (x + px + p' + 2)
    where
        (p', x) = if rx == r then (p, 2) else (1, 0)
        (rx, px) = (r + length cmts - 1, length . last $ cmts)
        cmts = split '\n' xx
        (xx, rem) = parseMultiComment xs

tokenize ('/' : '/' : xs) r p = tokenize rem r 1
    where
        (_, rem) = parseComment xs

tokenize (':' : xs) r p = Token COLUMN r p : tokenize xs r (p + 1)
tokenize ('-' : '>' : xs) r p = Token COLUMN r p : tokenize xs r (p + 1)
tokenize ('|' : xs) r p = Token BAR    r p : tokenize xs r (p + 1)
tokenize ('*' : xs) r p = Token STAR   r p : tokenize xs r (p + 1)
tokenize ('?' : xs) r p = Token QMARK  r p : tokenize xs r (p + 1)
tokenize ('+' : xs) r p = Token PLUS   r p : tokenize xs r (p + 1)
tokenize (';' : xs) r p = Token SEMI   r p : tokenize xs r (p + 1)
tokenize ('.' : xs) r p = Token SEMI   r p : tokenize xs r (p + 1)
tokenize ('(' : xs) r p = Token LPAREN r p : tokenize xs r (p + 1)
tokenize (')' : xs) r p = Token RPAREN r p : tokenize xs r (p + 1)

tokenize ('\'' : xs) r p = Token (Literal t) r p : tokenize (tail rem) r (p + 2 + length t)
                        where (t, rem) = span (/= '\'') xs

tokenize (x : xs) r p | x == '\n' = tokenize xs (r + 1) 1

tokenize (x : xs) r p | isSpace x = tokenize xs r (p + 1)

tokenize (x : xs) r p | isTerminalHead x = Token (Terminal $ x : t) r p : tokenize rem r (p + length t + 1)
                        where (t, rem) = span isTerminalChar xs

tokenize (x : xs) r p | isNonTerminalHead x = Token (NonTerminal $ x : t) r p : tokenize rem r (p + length t + 1)
                        where (t, rem) = span isNonTerminalChar xs

tokenize (x : xs) r p = error $ "unrecognized char: " ++ show r ++ " at " ++ show p ++ ": " ++ show x

lexing :: String -> [Token]
lexing x = tokenize x 1 1