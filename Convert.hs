module Convert where
import ADT
import Data.List (find)
import Data.Maybe (fromJust)

convertGrammar :: Grammar -> Grammar
convertGrammar = foldl (\acc x -> let (r, gg) = convertRule x acc in
                      r : gg) []

convertRule :: Rule -> Grammar -> (Rule, Grammar)
convertRule r@(TerminalRule _ _) g = (r, g)
convertRule (NonTerminalRule n br) g =
            let (br', g') = convertBranches br n g in
                (NonTerminalRule n br', g')

convertBranches :: [[Term]] -> String -> Grammar -> ([[Term]], Grammar)
convertBranches ts n g =
    if length ts == 1 then 
        foldl (\(ac1, ac2) t -> let (rt, rg) = reduceOnTermList t n ac2 in
                    (ac1 ++ [rt], ac2 ++ rg)) ([], g) ts
    else
        foldl (\(ac1, ac2) (t, u) -> let (rt, rg) = reduceOnTermList t (n ++ "@" ++ show u) ac2 in
                    (ac1 ++ [rt], ac2 ++ rg)) ([], g) (zip ts [1..])

reduceOnTermList :: [Term] -> String -> Grammar -> ([Term], Grammar)
reduceOnTermList ts n g =
    let z = zip ts [1..] in
        let zz = map (\((f, _), y) -> convertFactor f (n ++ "#" ++ show y) g) z in
            let zx = map (\((_, x), _) -> x) z in
                let ff = map fst zz in
                    let fx = zip ff zx in
                        let zu = zipWith (\ x y -> convertTerm x (n ++ "#" ++ show y) g) fx [1..] in
                            let gx = ff `zip` map snd zu in
                    let gg = concatMap snd zz in
                        (map (\(x, y) -> (renameFactor x y, None)) gx, gg ++ concatMap fst zu)

convertTermList :: [Term] -> String -> Grammar -> Grammar
convertTermList t n g =
    let (ts, g1) = reduceOnTermList t n g in
    concatMap fst (foldl (\acc x -> acc ++ [convertTerm x n g1]) [] ts)

convertFactor :: Factor -> String -> Grammar -> (Factor, Grammar)
convertFactor t@(TerminalTerm _) _ _ = (t, [])
convertFactor n@(NonTerminalTerm _) _ _ = (n, [])
convertFactor lt@(LiteralTerm l) n g =
    if any (containsLiteral l) g
        then (NonTerminalTerm (fromJust $ ruleName <$> find (containsLiteral l) g), [])
        else (NonTerminalTerm n, [TerminalRule n l])
convertFactor (Group xs) n g =
        let n' = n ++ "_Group" in
        let (tt, ng) = convertBranches xs n g in
            (NonTerminalTerm n', ng ++ [NonTerminalRule n' tt])
convertFactor Epsilon _ _ = (Epsilon, [])

convertTerm :: Term -> String -> Grammar -> (Grammar, Maybe String)
convertTerm (f, s) n g = case s of
    -- do nothing
    None -> ([], Nothing)
    -- S = A B* C -> S = A X C, X = ε | B X., X is returned
    Star -> let n' = n ++ "_Star" in
        ([NonTerminalRule n' [[(Epsilon, None)], [(f, None), (NonTerminalTerm n', None)]]], Just n')
    -- S = A B? C -> S = A X C, X = ε | B., X is returned
    QMark -> let n' = n ++ "_Option" in
        ([NonTerminalRule n' [[(Epsilon, None)], [(f, None)]]], Just n')
    -- S = A B+ -> S = A X, X = B FB, FB = X | ε., X is returned
    Plus -> let n1 = n ++ "_Plus1" in
            let n2 = n ++ "_Plus2" in
            ([
                NonTerminalRule n1 [[(f, None), (NonTerminalTerm n2, None)]],
                NonTerminalRule n2 [[(NonTerminalTerm n1, None)], [(Epsilon, None)]]
            ], Just n1)

renameFactor :: Factor -> Maybe String -> Factor
renameFactor f Nothing = f
renameFactor (TerminalTerm _) (Just n) = TerminalTerm n
renameFactor (NonTerminalTerm _) (Just n) = NonTerminalTerm n
renameFactor Epsilon _ = Epsilon
renameFactor _ _ = error "renameFactor over illegal factor"