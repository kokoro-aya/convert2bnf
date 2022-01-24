module ADT where

-- A typical antlr grammar contains repetition and branch as well as groups

type Grammar = [Rule]
data Rule = TerminalRule String String 
             | NonTerminalRule String Branches
             deriving (Show, Eq)

-- Term act as pointer that point to rules, in order to avoid ambiguity and remove cases such as foo*?, terms are organized
-- in a style such as term -> factor. The grammar was then proceeded to eliminate left factor. 
{-
    Here is the original grammar:
        term_list: term+;
        term: factor | factor '?' | factor '*' | factor '+';
        factor: TERMINAL | NON_TERMINAL | LITERAL | '(' alt ')';

    After eliminating left factor:
        term_list: term+;
        term: factor symbol;
        factor: TERMINAL | NON_TERMINAL | LITERAL | '(' alt ')';
        symbol: ε | '?' | '*' | '+';
-}
-- The data structure was therefore modified to lift the symbol out of factor/term and loop on term/group.

type Branches =  [[Term]] -- the outer list stand for different branches of the rule/derivation and the inner list stand for a sequence of terms in this branch

data Factor = TerminalTerm String
          | NonTerminalTerm String
          | LiteralTerm String
          | Group Branches
          | Epsilon
          deriving (Show, Eq)

type Term = (Factor, Symbol)

data Symbol = None | QMark | Star | Plus deriving (Show, Eq)

-- A BNF grammar is constituted only with terminal and non terminal terms and is not allowed to contain list of terms in a branch.
-- Symbols are not allowed on BNF grammar.

containsLiteral :: String -> Rule -> Bool
containsLiteral s (TerminalRule _ t) = s == t
containsLiteral _ _ = False

ruleName :: Rule -> String
ruleName (TerminalRule n _) = n
ruleName (NonTerminalRule n _) = n

isTerminalRule :: Rule -> Bool
isTerminalRule (TerminalRule _ _) = True
isTerminalRule _ = False