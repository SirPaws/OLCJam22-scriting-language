
#include <ctype.h>

#include "pstring.h"
#include "pstacktrace.h"
#include "stretchy_buffer.h"

#include "tokeniser.h"

#if PSTD_MSVC
#define cleanup(x) _Pragma("message(\"the gnu attribute cleanup is not available on the MVSC compiler! this will not work as expected!\")")
#else
#define cleanup(x) __attribute__((cleanup(x)))
#endif

typedef struct location_t location_t;
struct location_t {
    usize offset;
    usize line, column;
};

struct lexical_context {
    union {
        FILE *ptr;
        struct {
            char *buffer;
            const char *end;
            const char *current;
        };
    };
    location_t loc;
    const char *name;

    token_t current_token;
    token_t next_token;
    void (*step_back)(void);
    int (*read_char)(void);
} file ={.loc={0, 1, 1}};

PSTD_UNUSED static inline void step_backf(void);
PSTD_UNUSED static inline void step_backb(void);
PSTD_UNUSED static inline int read_charf(void);
PSTD_UNUSED static inline int read_charb(void);


void clean_context(void) {
    if (!file.step_back) return;
    
    if (file.step_back == step_backf) {
        fclose(file.ptr);
    } else {
        pfree(file.buffer);
    }

    file = (struct lexical_context){.loc={0, 1, 1}};
}

void terminate_lexer(void) {
    clean_context();
}

void read_file(const char *filepath) {
    clean_context();
    FILE *fptr = NULL;
    errno_t er = fopen_s(&fptr, filepath, "r");
    if (er) {
        panic("could not open %s file!", filepath);
    }

    file.ptr = fptr;
    file.step_back = step_backf;
    file.read_char = read_charf;
    file.name = filepath;
}

void read_buffer(const char *name, usize length, const char buffer[length]) {
    clean_context();

    file.buffer = pallocate(length + 1);
    passert(file.buffer);
    memcpy(file.buffer, buffer, length);
    file.buffer[length] = '\0';
    file.end = file.buffer + length;
    file.current = file.buffer;

    file.step_back = step_backb;
    file.read_char = read_charb;
    file.name = name;
}

PSTD_UNUSED static inline int read_charf(void) {
    return getc(file.ptr);
}

PSTD_UNUSED static inline int read_charb(void) {
    if (file.current >= file.end)
        return EOF;
    return *file.current++;
}

PSTD_UNUSED static inline void step_backf(void) {
    fpos_t pos;
    if (fgetpos(file.ptr, &pos)) {
        panic("could not get file position");
    }
    pos -= 1;
    if (fsetpos(file.ptr, &pos)) {
        panic("could not set file position");
    }
}

PSTD_UNUSED static inline void step_backb(void) {
    if (file.current - 1 < file.buffer) return;
    file.current--;
}

static inline void free_simple_stretchy_buffer(void *buf) {
    if (*(void**)buf) psb_free(*(void**)buf);
}

#define pchar_anyof(c, ...) pchar_anyof(c, sizeof((char[]){__VA_ARGS__}), (char[]){__VA_ARGS__})


int read_char() {
    int c = file.read_char();
    file.loc.offset++;
    file.loc.column++;
    if (c == '\n') {
        file.loc.line++;
        file.loc.column = 1;
    }
    return c;
}


token_t read_token(void) {
     
#define KEYWORD(enum, string)\
    static const pstring_t kw##enum = pcreate_const_string(string);
#include "tokens.h"

    static pbool_t is_character[255] = {
        ['.'] = true,
        [','] = true,
        [':'] = true,
        [';'] = true,
        ['['] = true,
        [']'] = true,
        ['{'] = true,
        ['}'] = true,
        ['('] = true,
        [')'] = true,
        ['+'] = true,
        ['-'] = true,
        ['*'] = true,
        ['/'] = true,
        ['%'] = true,
        ['='] = true,
        ['>'] = true,
        ['<'] = true,
        ['|'] = true,
        ['&'] = true,
        ['?'] = true,
        ['!'] = true,
    };

    cleanup(free_simple_stretchy_buffer)
    char *stretchy str = NULL;
    int c = 0;

    location_t start = file.loc;
    while ((c = read_char()) != EOF) {
        if (isspace(c)) continue;
        start = file.loc;

        if (c == '"') {
            psb_pushback(str, c);
            while ((c = read_char()) != '"') {
                if (c == '\n' || c == EOF)
                    panic("unterminated string");
            }

            psb_remove(str, str);
            psb_popback(str);

            usize length = psb_length(str);
            pstring_t string;
            if (length == 0)
                 string = pcopy_string(pstring("", 0));
            else string = pstring(psb_unstretch(str), length);
            
            return (token_t){
                .kind = LITERAL_STRING,
                .file_offset = start.offset,
                .line        = start.line,
                .column      = start.column,
                .file        = file.name,
                .sval        = string,
            };
        }

        if (isdigit(c)) {
            bool has_dot = false;
            do {
                psb_pushback(str, c);
                c = read_char();
                if (c == '.') {
                    has_dot = true;
                    psb_pushback(str, '.');
                    int next = read_char();
                    if (!isdigit(next)) {
                        file.loc.offset--;
                        file.loc.column--;
                        file.step_back();
                    }
                    c = next;
                }
            } while (isdigit(c));
            file.loc.offset--;
            file.loc.column--;
            file.step_back();
            
            char *p = str + psb_length(str);
            token_t token = {
                .kind = has_dot ? LITERAL_FLOAT : LITERAL_UINT,
                .file_offset = start.offset,
                .line        = start.line,
                .column      = start.column,
                .file        = file.name
            };
            if (has_dot)
                 token.fval = strtod(str, &p);
            else token.uval = strtoull(str, &p, 10);
            return token;
        }

        if (is_character[c]) {
            if (str) {
                file.step_back();
                file.loc.offset--;
                file.loc.column--;
                break;
            }
            

            if (pchar_anyof(c, '+', '-', '*', '/', '%', '=', '>', '<', '|', '&', '?')) {
                int next = read_char();

                if (c == '-' && next == '>') {
                    return (token_t){
                        .kind = ARROW,
                        .file_offset = start.offset,
                        .line        = start.line,
                        .column      = start.column,
                        .file        = file.name
                    };
                }

                if (next == '/' && c == next) {
                    // comment
                    c = next;
                    while ((c = read_char()) != '\n') {
                        if (c == EOF) break;
                    }
                    continue;
                }

                if (pchar_anyof(c, '+', '-', '|', '&', '=', '?') && c == next) {
                    // either ++, --, ||, &&, ==

                    // var ?= 3;
                    // if !?var var = 3;
                    int kind = 0;
                    switch (c) {
                    case '+': kind = INCREMENT;       break;
                    case '-': kind = DECREMENT;       break;
                    case '|': kind = BOOLEAN_OR;      break;
                    case '&': kind = BOOLEAN_AND;     break;
                    case '=': kind = EQUAL_EQUAL;     break;
                    case '?': kind = NULL_COALESCING; break;
                    }

                    return (token_t){
                        .kind = kind,
                        .file_offset = start.offset,
                        .line        = start.line,
                        .column      = start.column,
                        .file        = file.name
                    };
                }
                else if (pchar_anyof(c, '|', '&') || next != '=') {
                    file.step_back();
                    file.loc.offset--;
                    file.loc.column--;
                    goto return_single;
                }
                // +=, -=, *=, /=, %=, >=, <=
                int kind = 0;
                switch (c) {
                case '+': kind = ADD_EQUAL;
                case '-': kind = SUB_EQUAL;
                case '*': kind = MUL_EQUAL;
                case '/': kind = DIV_EQUAL;
                case '%': kind = MOD_EQUAL;
                case '>': kind = GREATER_THAN_EQUAL;
                case '<': kind = LESS_THAN_EQUAL;
                case '?': kind = NULL_ASSIGN;
                }

                return (token_t){
                    .kind = kind,
                    .file_offset = start.offset,
                    .line        = start.line,
                    .column      = start.column,
                    .file        = file.name
                };
            }

        return_single:
            return (token_t){
                .kind = c,
                .file_offset = file.loc.offset++,
                .line        = file.loc.line,
                .column      = file.loc.column++,
                .file        = file.name
            };
        }

        while (!isspace(c)) {
            psb_pushback(str, c);

            c = read_char();
            if (c == EOF) break;
            if (is_character[c]) {
                file.loc.offset--;
                file.loc.column--;
                file.step_back();
                break;
            }
        }
        if (str) break;
    }
    
    if (c == EOF && str == NULL) {
        return (token_t){
            .kind = END_OF_FILE
        };
    }

    pstring_t result = pstring(str, psb_length(str));
    int kind = IDENTIFIER;

#define KEYWORD(enum, str) \
    if (pcmp_string(result, kw##enum)) { kind = enum; goto found; }
#include "tokens.h"

    if (kind == IDENTIFIER) {
        usize length = psb_length(str);
        result = pstring(psb_unstretch(str), length);
        str = NULL;
    }

found:
    return (token_t) {
        .kind = kind,
        .file_offset = start.offset,
        .line        = start.line,
        .column      = start.column,
        .file        = file.name,
        .identifier  = result
    };
}

token_t consume_current_token(void) {
    file.current_token.kind = 0;
    return get_current_token();
}


token_t get_current_token(void) {
    if (file.current_token.kind != 0)
        return file.current_token;
    if (file.next_token.kind != 0) {
        file.current_token = file.next_token;
        file.next_token.kind = 0;
        return file.current_token;
    }

    return file.current_token = read_token();
}

token_t peek_next_token(void) {
    if (file.current_token.kind == 0)
        return get_current_token();
    if (file.next_token.kind == 0)
        return file.next_token = read_token();
    return file.next_token;
}

token_t expect_and_consume(usize kind) {
    static const pstring_t kinds[TOKEN_COUNT] = {
        [IDENTIFIER    ]  = pcreate_const_string("IDENTIFIER"    ),
        [LITERAL_INT   ]  = pcreate_const_string("LITERAL_INT"   ),
        [LITERAL_UINT  ]  = pcreate_const_string("LITERAL_UINT"  ),
        [LITERAL_FLOAT ]  = pcreate_const_string("LITERAL_FLOAT" ),
        [LITERAL_STRING]  = pcreate_const_string("LITERAL_STRING"),
        [INVALID_TOKEN ]  = pcreate_const_string("INVALID_TOKEN" ),
        [END_OF_FILE   ]  = pcreate_const_string("END_OF_FILE"   ),
        #define KEYWORD(enum, x) [enum] = pcreate_const_string(#enum),
        #include "tokens.h"
    };
    token_t tk = get_current_token();

    char expected_char[2] = {0};
    char actual_char[2] = {0};
    
    pstring_t actual;
    pstring_t expected;

    if (tk.kind < IDENTIFIER) {
        actual_char[0] = tk.kind;
        actual = pstring(actual_char, 1);
    }
    else actual = kinds[tk.kind];
    
    if (kind < IDENTIFIER) {
        expected_char[0] = kind;
        expected = pstring(expected_char, 1);
    }
    else expected = kinds[kind];


    if (tk.kind != kind) {
        panic("%s:%u:%u: expected '%S' but got '%S'!", tk.file, tk.line, tk.column, expected, actual);
    }
    return consume_current_token();
}

bool is(const token_t *const token, usize kind) {
    return token->kind == kind;
}
bool is_not(const token_t *const token, usize kind) {
    return !is(token, kind);
}




