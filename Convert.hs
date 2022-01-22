module Convert where
import ADT

-- convertGrammar :: Grammar -> Grammar -> Grammar

-- convertFactor :: Factor -> Grammar -> Grammar

convertTerm :: Term -> String -> Grammar -> (Grammar, String)
convertTerm (f, s) n g = case s of
    -- do nothing
    None -> ([], n)
    -- S = A B* C -> S = A X C, X = ε | B X., X is returned
    Star -> let n' = n ++ "_st" in
        ([NonTerminalRule n' [[(Epsilon, None)], [(f, None), (NonTerminalTerm n', None)]]], n')
    -- S = A B? C -> S = A X C, X = ε | B., X is returned
    QMark -> let n' = n ++ "_op" in
        ([NonTerminalRule n' [[(Epsilon, None)], [(f, None)]]], n')
    -- S = A B+ -> S = A X, X = B FB, FB = X | ε., X is returned
    Plus -> let n1 = n ++ "_pl1" in
            let n2 = n ++ "_pl2" in
            ([
                NonTerminalRule n1 [[(f, None), (NonTerminalTerm n2, None)]],
                NonTerminalRule n2 [[(NonTerminalTerm n1, None)], [(Epsilon, None)]]
            ], n1)