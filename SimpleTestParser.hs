import Parser (parseTopLevel)

testCode = "declaration: constant_declaration\n           | variable_declaration\n        /* | lateinit_var_declaration */\n           | computed_var_declaration\n           | typealias_declaration\n           | function_declaration\n           | enum_declaration\n           | struct_declaration\n           | prototype_declaration\n           | extension_declaration\n           | subscript_declaration // only in enum body or struct body\n           | initializer_declaration\n           ;\n\ncode_block: '{' statements? '}';\n\nconstant_declaration: 'cst' constant_name type_annotation? initializer (getter_block | code_block)?\n                    | 'cst' pattern_initializer_list\n                    ;\n"

test2Code = "variable_declaration: 'var' variable_name\n                      (type_annotation | initializer | type_annotation initializer)\n                      (getter_setter_block | get_set_block | code_block)?\n                    | 'var' pattern_initializer_list // must be [identifier_pattern]\n                    ;\n                    // Should not have getter_setter_block if out of struct"

test3Code = "getter_clause: 'get' code_block?;\nsetter_clause: 'set' setter_name? code_block;\nget_set_block: '{' 'get' ','? 'set'? '}'\n             | '{' 'set' ','? 'get' '}'\n             ;\nsetter_name: '(' IDENTIFIER ')';\n\nconstant_name: IDENTIFIER;\nvariable_name: IDENTIFIER;"

test4Code = "integer_literal: DECIMAL_LITERAL | HEXADECIMAL_LITERAL | OCTAL_LITERAL | BINARY_LITERAL;\ndouble_literal: DECIMAL_LITERAL '.' DECIMAL_LITERAL\n              ; // illegal: 0. or .5\nstring_literal: MULTILINE_STRING_LITERAL | STATIC_STRING_LITERAL | interpolated_string_literal;\ninterpolated_string_literal: STRING_HEAD expression (STRING_INTERM expression)* STRING_END;\n"

main = do
    putStrLn "Test case 1:\n----------------------------"

    putStrLn testCode
    putStrLn "----------------------------"
    print . parseTopLevel $ testCode

    putStrLn "\n\n"

    putStrLn "Test case 2:\n----------------------------"

    putStrLn test2Code
    putStrLn "----------------------------"
    print . parseTopLevel $ test2Code

    putStrLn "\n\n"

    putStrLn "Test case 3:\n----------------------------"

    putStrLn test3Code
    putStrLn "----------------------------"
    print . parseTopLevel $ test3Code

    putStrLn "\n\n"

    putStrLn "Test case 4:\n----------------------------"

    putStrLn test4Code
    putStrLn "----------------------------"
    print . parseTopLevel $ test4Code