module ADT where

-- A typical antlr grammar contains repetition and branch as well as groups
data Grammar = TerminalRule String String 
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

type Branches =  [[Term]]

data Factor = TerminalTerm String
          | NonTerminalTerm String
          | LiteralTerm String
          | Group Branches
          deriving (Show, Eq)

type Term = (Factor, Symbol)

data Symbol = None | QMark | Star | Plus deriving (Show, Eq)

-- A BNF grammar is constituted only with terminal and non terminal terms and is not allowed to contain list of terms in a branch.
