#ifndef KEYWORD
#define KEYWORD(enum, string)
#endif
#ifndef TOKEN
#define TOKEN(enum, example)
#endif


TOKEN(INCREMENT         , "++") // doesn't need to be strings
TOKEN(DECREMENT         , "--") // they aren't acturally used anywhere
TOKEN(BOOLEAN_OR        , "||") // it's just a way to tell the user
TOKEN(BOOLEAN_AND       , "&&") // what the enum refers to
TOKEN(EQUAL_EQUAL       , "==") // the only reason why these are strings
TOKEN(NULL_COALESCING   , "??") // is because I can't turn off trigraphs
TOKEN(ADD_EQUAL         , "+=") 
TOKEN(SUB_EQUAL         , "-=")
TOKEN(MUL_EQUAL         , "*=")
TOKEN(DIV_EQUAL         , "/=")
TOKEN(MOD_EQUAL         , "%=")
TOKEN(GREATER_THAN_EQUAL, ">=")
TOKEN(LESS_THAN_EQUAL   , "<=")
TOKEN(NULL_ASSIGN       , "?=")
TOKEN(ARROW             , "->")

KEYWORD(INT          , "int"    )
KEYWORD(UINT         , "uint"   )
KEYWORD(FLOAT        , "float"  )
KEYWORD(VOID         , "void"   )
KEYWORD(STRING       , "string" )
KEYWORD(BOOL         , "bool"   )
                     
KEYWORD(LET          , "let"    )
KEYWORD(MUT          , "mut"    )
KEYWORD(STRUCT       , "struct" )
KEYWORD(FN           , "fn"     )
KEYWORD(EXTERN       , "extern" )
                     
KEYWORD(IF           , "if"     )
KEYWORD(ELSE         , "else"   )
KEYWORD(FOR          , "for"    )
KEYWORD(WHILE        , "while"  )
KEYWORD(DO           , "do"     )
KEYWORD(DEFER        , "defer"  )
KEYWORD(RETURN       , "return" )
                     
KEYWORD(AS           , "as"     )
KEYWORD(REF          , "ref"    )
KEYWORD(LITERAL_TRUE , "true"   ) 
KEYWORD(LITERAL_FALSE, "false"  ) 


#undef KEYWORD
#undef TOKEN
