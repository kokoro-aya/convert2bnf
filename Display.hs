import ADT
import Data.List (intercalate)

displaySymbol :: Symbol -> String
displaySymbol None = ""
displaySymbol QMark = "?"
displaySymbol Star = "*"
displaySymbol Plus = "+"

displayFactor :: Factor -> String
displayFactor (TerminalTerm s) = s
displayFactor (NonTerminalTerm s) = s
displayFactor (LiteralTerm s) = s
displayFactor (Group br) = "(" ++ displayBranches br ++ ")"
displayFactor Epsilon = ""

displayBranches :: Branches -> String
displayBranches = intercalate "\n" . map (unwords . map displayTerm)

displayTerm :: Term -> String
displayTerm (f, s) = displayFactor f ++ displaySymbol s

displayRule :: Rule -> String
displayRule (TerminalRule n t) = n ++ " -> " ++ t ++ "."
displayRule (NonTerminalRule n bt) = n ++ " -> " ++ displayBranches bt ++ "."