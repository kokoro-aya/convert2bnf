module Token where


data Token = Token {
    tokenType :: TokenType,
    line :: Int,
    pos :: Int
} deriving (Eq)

instance Show Token where
    show (Token t r c) = show t ++ " at: R:" ++ show r ++ ",C:" ++ show c ++ ".\n"

data TokenType = 
      COLUMN | BAR | STAR | QMARK | PLUS | LPAREN | RPAREN | SEMI
    | NonTerminal String 
    | Terminal String
    | Literal String
    deriving (Eq, Show)