module Convert where
import ADT
import Data.List (find)
import Data.Maybe (fromJust)

convertGrammar :: Grammar -> Grammar
convertGrammar = foldl (\acc x -> let (r, gg) = convertRule x acc in
                      gg ++ [r]) []

convertRule :: Rule -> Grammar -> (Rule, Grammar)
convertRule r@(TerminalRule _ _) g = (r, g)
convertRule (NonTerminalRule n br) g =
        let (br', g') = convertBranches br n g in
            (NonTerminalRule n br', g')

convertBranches :: [[Term]] -> String -> Grammar -> ([[Term]], Grammar)
convertBranches ts n g =
    foldl (\(ac1, ac2) t -> let (rt, rg) = reduceOnTermList t n ac2 in
                    (ac1 ++ [rt], ac2 ++ rg)) ([], g) ts

reduceOnTermList :: [Term] -> String -> Grammar -> ([Term], Grammar)
reduceOnTermList ts n g =
    let z = zip ts [1..] in
        let zz = map (\((f, _), y) -> convertFactor f (n ++ "#" ++ show y) g) z in
            let ff = map (\x -> (fst x, None)) zz in
                let gg = concatMap snd zz in
                    (ff, gg)

convertTermList :: [Term] -> String -> Grammar -> Grammar
convertTermList t n g =
    let (ts, g1) = reduceOnTermList t n g in
    concatMap fst (foldl (\acc x -> acc ++ [convertTerm x n g1]) [] t)

convertFactor :: Factor -> String -> Grammar -> (Factor, Grammar)
convertFactor t@(TerminalTerm _) _ _ = (t, [])
convertFactor n@(NonTerminalTerm _) _ _ = (n, [])
convertFactor lt@(LiteralTerm l) n g =
    if any (containsLiteral l) g
        then (NonTerminalTerm (fromJust $ ruleName <$> find (containsLiteral l) g), [])
        else (NonTerminalTerm n, [TerminalRule n l])
convertFactor (Group xs) n g =
        let n' = n ++ "_g" in
        let (tt, ng) = convertBranches xs n g in
            (NonTerminalTerm n', ng ++ [NonTerminalRule n' tt])
convertFactor Epsilon _ _ = (Epsilon, [])

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