import Display (display)
import Parser (parseTopLevel)
import Convert (convertGrammar)

main = do
    putStrLn "enter your input file"
    input <- getLine
    file <- readFile input
    let g = parseTopLevel file
    putStrLn "------------------------"
    putStrLn "    Original grammar"
    putStrLn "------------------------"
    putStrLn . display False $ g

    let g2 = convertGrammar g
    putStrLn "------------------------"
    putStrLn "     Output grammar"
    putStrLn "------------------------"
    putStrLn . display True $ g2
