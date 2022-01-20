module ADT where

-- A EBNF grammar contains repetition and branch as well as groups
data Grammar = TerminalRule String 
             | NonTerminalRule [Term]

-- Term act as pointer that point to rules
data Term = 
          TerminalTerm String
        | NonTerminalTerm String
        | Branch [Term]
        | Optional Term
        | Many Term
        | Many1 Term

-- A BNF grammar is constituted only with terminal and non terminal terms and is not allowed to contain subarrays.
