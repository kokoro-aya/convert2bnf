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

isSemi SEMI = True
isSemi _ = False

isRParen RPAREN = True
isRParen _ = False

isLParen LPAREN = True
isLParen _ = False

isPlus PLUS = True
isPlus _ = False

isQmark QMARK = True
isQmark _ = False

isStar STAR = True
isStar _ = False

isBar BAR = True
isBar _ = False

isColumn COLUMN = True
isColumn _ = False

isNonTerminal (NonTerminal _) = True
isNonTerminal _ = False

isTerminal (Terminal _) = True
isTerminal _ = False

isLiteral (Literal _) = True
isLiteral _ = False

getValue' (NonTerminal s) = s
getValue' (Terminal s) = s
getValue' (Literal s) = s
getValue' _ = error "Not a value token"

getValue = getValue' . tokenType

-- TODO : replace error with a maybe