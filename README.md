# L25CompilerC
Implement an L25 Compiler in C.
## EBNF
\<program\> = "program" \<ident\> "{" {\<func_def\>} "main" "{" \<stmt_list\> "}" "}"  

\<func_def\> = "func" \<ident\> "(" [\<param_list\>] ")" "{" \<stmt_list\> "return" \<expr\> ";" "}"  

\<param_list\> = [ "" | "str" ] \<ident\> { "," [ "*" | "str" ] \<ident\> }  

\<stmt_list\> = \<stmt\> ";" {\<stmt\> ";" }  
\<stmt\> = \<declare_stmt\> | \<assign_stmt\> | \<if_stmt\> | \<while_stmt\> | \<input_stmt\> | \<output_stmt\> | \<func_call\> | \<alloc_stmt\> | \<free_stmt\> | \<return_stmt\> | \<update_stmt\>  

\<declare_stmt\> = "let" [ "*" | "str" | "map" | "set" ] \<ident\> [ "=" \<expr\> ]  
\<assign_stmt\> = \<ident\> "=" \<expr\> | \<dereference\> "=" \<expr\>  
\<if_stmt\>= "if" "(" \<bool_expr\> ")" "{" \<stmt_list\> "}" [ "else" "{" \<stmt_list\> "}"]  
\<while_stmt\> = "while" "(" \<bool_expr\> ")" "{" \<stmt_list\>"}"  
\<func_call\> = \<ident\> "(" [\<arg_list\>] ")"  
\<arg_list\> = \<expr\> { "," \<expr\> }  
\<input_stmt\> = "input" "(" \<ident\> {"," \<ident\>} ")"  
\<output_stmt\> = "output" "(" \<expr\> { "," \<expr\> } ")"  
\<alloc_stmt\> = "alloc" "(" \<ident\> "," \<expr\> ")"  
\<free_stmt\> = "free" "(" \<ident\> ")"  
\<return_stmt\> = "return" \<expr\>  
\<update_stmt\> = \<ident\> "." ( "insert" "(" \<expr\> [ "," \<expr\> ] ")" | "delete" "(" \<expr\> ")" ) | \<ident\> "[" \<expr\> "]" "=" \<expr\>  

\<bool expr\>= \<expr\> ("==" | "!=" | "<" | "<=" | ">" | ">=") \<expr\>  
\<expr\> = [ "+" | "-" ] \<term\> { ("+" | "-") \<term\> }  
\<term\> = \<factor\> {("*" | "/") \<factor\> }  
\<factor\>= \<ident\> | \<number\> | "(" \<expr\> ")" | \<func_call\> | \<reference\> | \<dereference\> | \<string\> | \<traversal\> | \<size\> | \<look_up\> | \<find\>  
\<reference\> = & \<ident\>  
\<dereference\> = * \<ident\> | \<ident\> "[" \<expr\> "]"  
\<find\> = \<ident\> "." "find" "(" \<expr\> ")"  
\<look_up\> = \<ident\> "[" \<expr\> "]"  
\<traversal\> = \<ident\> "." "traverse" "(" ")"  
\<size\> = \<ident\> "." "size" "(" ")"  

\<ident\> = \<letter\> {\<letter\> | \<digit\>}  
\<number\> = \<digit\> {\<digit\>}  
\<letter\> = "a" | "b" | ... | "z" | "A" | "B" | ... | "Z"  
\<digit\> = "0" | "1" | ... | "9"  
\<string\> = """ { \<char\> } """  
\<char\> = set(ASCII) - { """ }  
\<comment\> = "#" { set(ASCII) }  
