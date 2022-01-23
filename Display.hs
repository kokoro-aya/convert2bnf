module Display where

import ADT
import Data.List (intercalate)
import Data.Char (toUpper, toLower)

displaySymbol :: Symbol -> String
displaySymbol None = ""
displaySymbol QMark = "?"
displaySymbol Star = "*"
displaySymbol Plus = "+"

displayFactor :: Bool -> Factor -> String
displayFactor u (TerminalTerm s) = if u then toLowercase s else s
displayFactor u (NonTerminalTerm s) = if u then toUppercase s else s
displayFactor _ (LiteralTerm s) = "'" ++ s ++ "'"
displayFactor u (Group br) = "(" ++ displayBranches u br ++ ")"
displayFactor _ Epsilon = ""

displayBranches :: Bool -> Branches -> String
displayBranches u = intercalate " | " . map (unwords . map (displayTerm u))

displayTerm :: Bool -> Term -> String
displayTerm u (f, s) = displayFactor u f ++ displaySymbol s

displayRule :: Bool -> Rule -> String
displayRule u (TerminalRule n t) = 
    let n' = if u then toLowercase n else n in
        n' ++ " -> '" ++ t ++ "'."
displayRule u (NonTerminalRule n bt) = 
    let n' = if u then toUppercase n else n in
        n' ++ " -> " ++ displayBranches u bt ++ "."

display :: Bool -> Grammar -> String
display u = intercalate "\n" . map (displayRule u)

toUppercase :: String -> String
toUppercase "" = ""
toUppercase (x:xs) = toUpper x: xs

toLowercase :: String -> String
toLowercase "" = ""
toLowercase (x:xs) = toLower x: xs