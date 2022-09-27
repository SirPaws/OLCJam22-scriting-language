#pragma once
#ifndef TOKENISER_HEADER
#define TOKENISER_HEADER

#include <limits.h>
#include "plimits.h"
#include "pstring.h"


typedef struct token_t token_t;
struct token_t {
    enum TokenKinds : usize {
        IDENTIFIER = plimits(unsigned char).max + 1,
        LITERAL_INT,
        LITERAL_UINT,
        LITERAL_FLOAT,
        LITERAL_STRING,
    #define KEYWORD(enum, str) enum,
    #define TOKEN(enum, example) enum,
    #include "tokens.h"
    
        END_OF_FILE,
        INVALID_TOKEN,
        TOKEN_COUNT
    } kind;
    size_t line, column;
    usize file_offset;
    const char *file;
    union {
        pstring_t identifier;
        isize     ival; 
        usize     uval; 
        f64       fval;
        pstring_t sval;
    };
};

void read_file(const char *filepath);
void read_buffer(const char *name, usize length, const char buffer[length]);
void terminate_lexer(void);

token_t get_current_token(void);
token_t peek_next_token(void);
token_t consume_current_token(void);
token_t expect_and_consume(usize);


bool is(const token_t *const, usize);
bool is_not(const token_t *const, usize);

#endif
