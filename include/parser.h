#include <stdbool.h>
#include <limits.h>

#include "pio.h"
#include "pstacktrace.h"

#include "tokeniser.h"

//TODO: parse function declarations
//TODO: parse statements
//TODO: parse expressions

typedef struct parameter_t parameter_t;
struct parameter_t {
    usize                hash;
    pstring_t            name;
    struct type_t       *type;
    struct expression_t *value;
};

typedef struct named_parameter_node_t named_parameter_node_t;
typedef struct named_parameter_map_t named_parameter_map_t;
struct named_parameter_map_t {
    usize num_used_buckets;
    usize buckets_with_values[128];
    struct named_parameter_node_t {
        parameter_t parameter;
        named_parameter_node_t *next;
    } *nodes[128];
};

typedef struct parameter_list_t parameter_list_t;
struct parameter_list_t {
    usize num_parameters;
    usize num_named_parameters;
    named_parameter_map_t named_parameters; 
    usize num_unnamed_parameters;
    parameter_t *unnamed_parameters;
};

parameter_list_t parse_parameter_list(void);

typedef struct overload_t overload_t;
typedef union  value_t value_t;
typedef struct field_t field_t;
typedef struct type_t type_t;
struct type_t {
    enum {
        tyVOID     ,
        tyINT      , 
        tyUINT     , 
        tyBOOL     ,
        tyFLOAT    ,
        tySTRING   , 
        tyARRAY    , 
        tyREF      , 
        tyPTR      , 
        tyNULLABLE ,
        tyFUNCTION ,
        tySTRUCTURE,
        tyNUM_BASE_TYPES = tyARRAY,
    } kind;
    token_t begin, end;
    bool is_immutable;
    union {
        struct {
            type_t *type;
            usize size; bool is_dynamic; 
        } arr;
        type_t *ref, *ptr, *nullable;
        struct {
            pstring_t name;
            usize num_fields;
            struct field_t { 
                bool is_private;
                pstring_t name; 
                type_t *type;
            } *fields;
        };
        struct {
            parameter_list_t parameters;
            type_t *return_type;
        };
    };

    enum {
        ovNOT,               // fn  !(in_t) : out_t
        ovNEGATE,            // fn  -(in_t) : out_t
        ovSIGN,              // fn  +(in_t) : out_t
        ovSUBSCRIPT,         // fn [](in_t, index_t) : out_t
        ovMULTIPLY,          // fn  *(in_t, lhs_t) : out_t
        ovDIVIDE,            // fn  /(in_t, lhs_t) : out_t
        ovMODULO,            // fn  %(in_t, lhs_t) : out_t
        ovADDITION,          // fn  +(in_t, lhs_t) : out_t
        ovSUBTRACT,          // fn  -(in_t, lhs_t) : out_t
        ovEQUALITY,          // fn ==(in_t, lhs_t) : out_t
        ovGREATERTHANEQUAL,  // fn >=(in_t, lhs_t) : out_t
        ovLESSTHANEQUAL,     // fn <=(in_t, lhs_t) : out_t
        ovGREATERTHAN,       // fn  >(in_t, lhs_t) : out_t
        ovLESSTHAN,          // fn  <(in_t, lhs_t) : out_t
        ovUSERCAST,          // fn as(in_t) : out_t
        ovCOUNT,
    };

    struct overload_t {
        type_t               *right;
        struct declaration_t *function;
        overload_t           *next;
    } *overloads[ovCOUNT];

    union value_t {
        isize     ival;
        usize     uval;
        f64       fval;
        bool      bval;
        pstring_t sval;
        void     *memory;
        value_t *arr, *ref, *ptr, *structure;
    } default_value;
};

type_t *parse_typename(void);
type_t *make_type(type_t *type_stub, type_t *prev_type);

bool is_overloadable(type_t *ty);

/*

   struct x { ... };

   fn thing {
        struct y { ... };
   }

   let val : y;


*/
typedef struct declaration_t declaration_t;
struct declaration_t {
    enum {
        declSTRUCTURE, declFUNCTION,
        declMUTABLE,   declIMMUTABLE, 
        declPARAMETER, declOVERLOAD,
    } kind;
    
    token_t begin, end;
    // filled by the semantic analyser
    struct scope_t *scope;
    union {
        type_t structure;
        struct {
            bool is_external;
            pstring_t name;
            parameter_list_t parameters;
            type_t *return_type;
            struct statement_t *function_body;
        };
        struct {
            type_t              *type;
            pstring_t            identifier;
            struct expression_t *expression;
        };
    };
};

declaration_t *parse_declaration(void);
declaration_t *parse_structure_definition(void);
declaration_t *make_declaration(declaration_t *);

typedef struct block_item_t block_item_t;
struct block_item_t {
    enum { itmDECLARATION, itmSTATEMENT } kind;
    union { 
        declaration_t *declaration; 
        struct statement_t *statement; 
    };
};

typedef struct statement_t statement_t;
struct statement_t { 
    enum {
        stmtEXPRESSION,
        stmtRETURN,
        stmtBLOCK,
        stmtDEFER,
        stmtWHILE,
        stmtNOOP,
        stmtFOR,
        stmtIF,
        stmtDO,
    } kind;

    token_t begin, end;
    // filled by the semantic analyser
    struct scope_t *scope;
    union {
        struct expression_t *expression, *sreturn;
        struct {
            usize num_items;
            block_item_t *items;
        } block;
        statement_t *defer;
        struct {
            struct expression_t *condition;
            statement_t *block;
        } swhile, sdo;
        struct {
            struct expression_t *condition;
            statement_t *true_branch;
            statement_t *false_branch;
        } sif;
        struct {
            declaration_t *declaration;
            struct expression_t *condition, *increment;
            statement_t *block;
        } sfor;
    };
};

statement_t *make_statement(statement_t *);
statement_t *parse_statement(void);

typedef struct expression_t expression_t;
struct expression_t {
    enum {
        exprLITERAL,
        exprPREFIX,
        exprPOSTFIX,
        exprBINARY,
        exprASSIGNMENT,
        exprCOMMA,
        exprLAMBDA,
        exprCAST,
        exprCALL,
        exprSUBSCRIPT,
        exprMEMBER_ACCESS,
        exprPOINTER_MEMBER_ACCESS,
        exprCOMPOUND_LITERAL,
    } kind;
    bool is_constant;
    token_t begin, end;

    // this is filled by the semantic analyser
    type_t *type;
    
    bool is_mutable;
    union {
        struct {
            enum {
                litINT,
                litUINT,
                litFLOAT,
                litSTRING,
                litBOOL,
                litIDENTIFIER,
            } lit_kind;
            union {
                value_t   value;
                pstring_t identifier;
            };
        };
        struct {
            type_t *type;
            usize num_values;
            expression_t **values;
        } compound;
        struct {
            expression_t *obj, *expression;
        };
        struct {
            enum {
                exprEQUALITY           = EQUAL_EQUAL,
                exprGREATER_THAN_EQUAL = GREATER_THAN_EQUAL,
                exprLESS_THAN_EQUAL    = LESS_THAN_EQUAL,
                exprGREATER_THAN       = '<',
                exprLESS_THAN          = '>',
                exprNULL_COALESCING    = NULL_COALESCING,
                exprOR                 = BOOLEAN_OR,
                exprAND                = BOOLEAN_AND,
                exprADDITION           = '+',
                exprSUBTRACTION        = '-',
                exprMULTIPLICATION     = '*',
                exprDIVISION           = '/',
                exprMODULO             = '%',
            } kind;
            expression_t *left, *right;
        } binary;
        struct {
            enum {
                exprNOT          = '!',
                exprNEGATE       = '-',
                exprSIGN         = '+',
                exprDEREFERENCE  = '*',
                exprREFERENCE    = '&',
                exprAS_REFERENCE = REF,
            } kind;
            expression_t *operand;
        } prefix;
        struct {
            enum {
                exprUNWRAP     = '!',
                exprNULLCHECK  = '?',
            } kind;
            expression_t *operand;
        } postfix;
        struct {
            expression_t *member_access_obj;
            bool is_integral;
            union { usize uval; pstring_t member; };
        };
        struct {
            expression_t *cast_expression;
            type_t *cast_type;
        };
        struct {
            expression_t *subscript_obj;
            expression_t *index;
        };
        struct {
            expression_t *called_obj;
            usize num_parameters;
            struct call_parameter_t {
                bool named;
                pstring_t name;
                expression_t *value;
            } *parameters;
        };
    };
};

expression_t *make_expression(expression_t *new);
expression_t *parse_and_fold_expression(void);

expression_t *parse_expression(void);
expression_t *parse_non_comma_expression(void);

// a = 3*4 + 1;
expression_t *parse_assignment_expression(void);
expression_t *parse_inquisitive_expression(void);   // && || ??
expression_t *parse_equality_expression(void);      // == >= <= < >
expression_t *parse_additive_expression(void);      //  +  -
expression_t *parse_multiplicative_expression(void);//  *  /  %
expression_t *parse_prefix_expression(void);        // ?$ -$ +$ &$ ref $
expression_t *parse_postfix_expression(void);       // $. $[] $()
expression_t *parse_primary_expression(void);

typedef struct node_t node_t;
struct node_t {
    usize     hash;
    pstring_t name;
    type_t   *value;
    node_t   *next;
};

typedef struct typemap_t typemap_t;
struct typemap_t {
    type_t base_types[tyNUM_BASE_TYPES];
    node_t *nodes[128];
};

union hash_value_t {
    char buf[sizeof(u64)];
    u64 value;
} extern hash_seed;
overload_t *make_overload(type_t *rhs, declaration_t *decl, overload_t *next);
typemap_t yoinky_sploink_types(void);
type_t *get_type(usize kind, pstring_t *name);
