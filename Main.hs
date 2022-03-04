import Display (display)
import Parser (parseTopLevel)
import Convert (convertGrammar)

main = do
    putStrLn "enter your input file"
    putStr "> "
    input <- getLine
    file <- readFile input
    let g = parseTopLevel file
    putStrLn "------------------------"
    putStrLn "    Original grammar"
    putStrLn "------------------------"
    putStrLn ""
    putStrLn . display False $ g

    putStrLn ""

    let g2 = convertGrammar g
    putStrLn "------------------------"
    putStrLn "     Output grammar"
    putStrLn "------------------------"
    putStrLn ""
    putStrLn . display True $ g2
