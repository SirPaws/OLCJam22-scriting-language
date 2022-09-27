
#pragma once
#include "general.h"
#include "stretchy_buffer.h"
#include "pstring.h"
#include "pio.h"
#include "pstacktrace.h"

#include <Windows.h>
#include <direct.h> // for _mkdir
#include <ctype.h>
#include <vadefs.h>

#if PSTD_MSVC
#define cleanup(x) _Pragma("message(\"the gnu attribute cleanup is not available on the MVSC compiler! this will not work as expected!\")")
#else
#define cleanup(x) __attribute__((cleanup(x)))
#endif

#ifndef ERROR_RED_CHANNEL
#define ERROR_RED_CHANNEL   255
#endif
#ifndef ERROR_GREEN_CHANNEL
#define ERROR_GREEN_CHANNEL 0
#endif
#ifndef ERROR_BLUE_CHANNEL
#define ERROR_BLUE_CHANNEL  0
#endif

#define ERROR_RGB                             \
    "%Cfg("                                   \
        PSTD_STRINGIFY(ERROR_RED_CHANNEL)  ","\
        PSTD_STRINGIFY(ERROR_GREEN_CHANNEL)","\
        PSTD_STRINGIFY(ERROR_BLUE_CHANNEL)    \
    ")"

PSTD_UNUSED static inline pbool_t read_word(FILE *file, pstring_t *result);
PSTD_UNUSED static inline pbool_t read_until(FILE *file, pstring_t *result, usize count, const char delim[count]);
PSTD_UNUSED static inline pbool_t unread_word(FILE *file, pstring_t word);
PSTD_UNUSED static inline pbool_t is_character(pstring_t str, char delim);
PSTD_UNUSED static inline pbool_t is_identifier(pstring_t str);

PSTD_UNUSED static inline FILE *open_file(const char *file_path);
PSTD_UNUSED static inline char *pstrndup(const char *str, usize length);
PSTD_UNUSED static inline void block_close(FILE **f);
PSTD_UNUSED static void print_error(const char *message, ...);



static inline void free_simple_stretchy_buffer(void *buf) {
    if (*(void**)buf) psb_free(*(void**)buf);
}

PSTD_UNUSED
static inline void step_back_file(FILE *file) {
    fpos_t pos;
    if (fgetpos(file, &pos)) {
        panic("could not get file position");
    }
    pos -= 1;
    if (fsetpos(file, &pos)) {
        panic("could not set file position");
    }
}
PSTD_UNUSED
static inline pbool_t read_word(FILE *file, pstring_t *result) {
    pfree_string(result);

    static pbool_t is_character[255] = {
        [','] = true,
        [':'] = true,
        [';'] = true,
        ['['] = true,
        [']'] = true,
        ['{'] = true,
        ['}'] = true,
        ['('] = true,
        [')'] = true,
    };

    cleanup(free_simple_stretchy_buffer)
    char *stretchy str = NULL;
    int c = 0;

    while ((c = getc(file)) != EOF) {
        if (is_character[c]) {
            if (str) {
                step_back_file(file);
                break;
            }
            char *r = malloc(2);
            r[0] = (char)c; r[1] = '\0';
            *result = pstring(r, 1);

            return true;
        }

        while (!isspace(c)) {
            psb_pushback(str, c);
            c = getc(file);
            if (c == EOF) break;
            if (is_character[c]) {
                step_back_file(file);
                break;
            }
        }
        if (str) break;
    }

    if (c == EOF && str == NULL) return false;
    if (isspace(c)) step_back_file(file);

    usize length = psb_length(str);
    *result = pstring(psb_unstretch(str), length);
    str = NULL;

    if (c == EOF) return false;
    return true;
}

PSTD_UNUSED static inline pbool_t read_until(FILE *file, pstring_t *result, usize count, const char delim[count]) {

    int c;
    cleanup(free_simple_stretchy_buffer)
    char *stretchy str = NULL;
    while ((c = getc(file)) != EOF) {
        for (usize i = 0; i < count; i++)
            if (c == delim[i]) goto break_loop;
        psb_pushback(str, c);
    }

break_loop:
    if (c == EOF || str == NULL) return false;
    step_back_file(file);

    result->length = psb_size(str);
    result->c_str = psb_unstretch(str);
    str = NULL;
    return true;
}
#define read_until(file, result, ...)\
    read_until(file, result, sizeof((const char[]){__VA_ARGS__}), (const char[]){__VA_ARGS__})

PSTD_UNUSED static inline pbool_t unread_word(FILE *file, pstring_t word) {
    for (char *last = word.c_str + word.length - 1; last != word.c_str - 1; last--) {
        int c;
        do {
            step_back_file(file);
            c = getc(file);
        } while (c != *last);
        step_back_file(file);
    }
    return true;
}

PSTD_UNUSED
static inline FILE *open_file(const char *file_path) {
    FILE *file = NULL;
    errno_t er = fopen_s(&file, file_path, "r");
    if (er) {
        print_error("could not open %s file!", file_path);
        return NULL;
    }
    return file;
}

PSTD_UNUSED
static inline void block_close(FILE **f) {
    if (*f) fclose(*f);
}

PSTD_UNUSED
static inline pbool_t is_character(pstring_t str, char delim) {
    if (str.length != 1) return false;
    return str.c_str[0] == delim;
}

PSTD_UNUSED static inline pbool_t is_identifier(pstring_t str) {
    if (!str.length || !str.c_str) return false;

    char *chr = str.c_str;
    if (!isalpha(*chr)) return false;
    
    while (++chr != str.c_str + str.length)
        if (!(isalnum(*chr) || *chr == '_')) return false;
    return true;
}

PSTD_UNUSED
static inline char *pstrndup(const char *str, usize length) {
    char *result = pallocate(length + 1);
    assert(result);
    assert(memcpy(result, str, length));
    result[length] = '\0';
    return result;
}

PSTD_UNUSED
static void print_error(const char *message, ...) {
    static pstd_stream_t err_stream = {
        .type          = STANDARD_STREAM,
        .flags         = STREAM_OUTPUT,
    };
    if (!err_stream.is_valid) {
        err_stream.stdout_handle = GetStdHandle(STD_ERROR_HANDLE);
        err_stream.is_valid = true;    
    }


    pbprintf(&err_stream, ERROR_RGB "error: ");

    va_list list;
    va_start(list, message);
    pvbprintf(&err_stream, message, list);
    va_end(list);
    pbprintf(&err_stream, "%Cc\n");
}
