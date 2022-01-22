module Parser where
import Control.Applicative (Alternative, empty, (<|>))
import Token (Token (line, pos, tokenType), isNonTerminal, isTerminal, isLiteral, isColumn, isBar, isSemi, isStar, isQmark, isPlus, isLParen, isRParen, getValue)
import ADT
import Lexer (lexing)

newtype Parser a = P ([Token] -> Either String (a, [Token]))

parse :: Parser a -> [Token] -> Either String (a, [Token])
parse (P p) = p

instance Functor Parser where
    fmap g (P p) = P (\s -> case p s of
                                Left s' -> Left s'
                                Right (a, s') -> Right (g a, s'))

instance Applicative Parser where
    pure a = P (\s -> Right (a, s))
    pg <*> px = P (\s -> case parse pg s of
                            Left s' -> Left s'
                            Right (g, s') -> parse (fmap g px) s')

instance Monad Parser where
    p >>= f = P (\s -> case parse p s of
                            Left s' -> Left s'
                            Right (a, s') -> parse (f a) s')

instance Alternative Parser where
    empty = P (\s -> Left "")
    p <|> q = P (\s -> case parse p s of
                            Left s' -> parse q s
                            Right (a, s') -> Right (a, s'))

item :: Parser Token
item = P (\s -> case s of
                    [] -> Left "Unexpected end of input"
                    (t:ts) -> Right (t, ts))

satisfy :: String -> (Token -> Bool) -> Parser Token
        -- Error message
satisfy m f = do t <- item
                 if f t then pure t else
                    let (r, p) = (line t, pos t) in
                        P (\_ -> Left $ "Error around " ++ show r ++ ":" ++ show p ++ ", " ++ m ++ ".")


choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

between :: Parser a -> Parser b -> Parser c -> Parser c
between p1 p2 p3 = do _ <- p1
                      x <- p3
                      _ <- p2
                      pure x

some :: Parser a -> Parser [a]
some p = do x <- p
            xs <- many p
            pure (x:xs)

many :: Parser a -> Parser [a]
many p = some p <|> pure []

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = do x <- p
                 xs <- many (do _ <- sep
                                p)
                 pure (x:xs)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do x <- p
                  xs <- many (do _ <- sep
                                 p)
                  pure (x:xs) <|> pure [x]

nonTerminalToken = satisfy "non-terminal token expected" (isNonTerminal . tokenType)
terminalToken = satisfy "terminal token expected" (isTerminal . tokenType)
literalToken = satisfy "literal token expected" (isLiteral . tokenType)

column = satisfy "column expected" (isColumn . tokenType)
bar = satisfy "bar expected" (isBar . tokenType)
star = satisfy "star expected" (isStar . tokenType)
qmark = satisfy "question mark expected" (isQmark . tokenType)
plus = satisfy "plus expected" (isPlus . tokenType)
lparen = satisfy "left parenthesis expected" (isLParen . tokenType)
rparen = satisfy "right parenthesis expected" (isRParen . tokenType)
semi = satisfy "semicolon expected" (isSemi . tokenType)

topLevel = some rule

rule = terminalRule <|> nonTerminalRule

terminalRule = do t <- terminalToken
                  _ <- column
                  l <- literalToken
                  _ <- semi
                  pure (TerminalRule (getValue t) (getValue l))

nonTerminalRule = do n <- nonTerminalToken
                     _ <- column
                     xs <- branches
                     _ <- semi
                     pure (NonTerminalRule (getValue n) xs)

branches = sepBy1 termlist bar

termlist = many term

term :: Parser Term
term = do f <- factor
          s <- symbol
          pure (f, s)

factor =  do TerminalTerm . getValue <$> terminalToken
      <|> do NonTerminalTerm . getValue <$> nonTerminalToken
      <|> do LiteralTerm . getValue <$> literalToken
      <|> do lparen
             xs <- branches
             rparen
             pure (Group xs)

symbol = do _ <- star
            pure Star
            <|> do _ <- qmark
                   pure QMark
                   <|> do _ <- plus
                          pure Plus
                          <|> pure None -- epsilon case

parseTopLevel :: String -> Grammar 
parseTopLevel = either (error . show) fst . parse topLevel . lexing