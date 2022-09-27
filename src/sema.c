
#include "parser.h"
#include "hash.h"
#include "stretchy_buffer.h"

typedef struct symbol_t symbol_t;
struct symbol_t {
    usize            hash;
    pstring_t        name;
    declaration_t   *value;
    struct scope_t  *scope;
    symbol_t        *next;
};

typedef struct scope_t scope_t;
struct scope_t {
    // symbol table
    usize num_used_buckets;
    usize buckets_with_values[128];
    symbol_t *nodes[128];
    scope_t *parent;
};

bool compare_types(type_t *left, type_t * right);
typemap_t yoinky_sploink_types(void);

type_t *tm_find_user_type(typemap_t *map, pstring_t name);

void add_declaration_to_scope(scope_t *scope, declaration_t *decl) {
    pstring_t name;
    if (decl->kind == declOVERLOAD  || decl->kind == declSTRUCTURE) return;
    if (decl->kind == declIMMUTABLE || decl->kind == declMUTABLE)
         name = decl->identifier;
    else name = decl->identifier;
    
    usize hash = phash(name.c_str, name.length, (usize)hash_seed.value);
    symbol_t *node = pallocate(sizeof *node);
    node->hash  = hash;
    node->name  = name;
    node->scope = scope;
    node->value = decl;
    node->next  = NULL;

    if (!scope->nodes[hash % 128]) {
        scope->nodes[hash % 128] = node;
        return;
    }

    symbol_t *list = scope->nodes[hash % 128];
    while (list->next) list = list->next;
    list->next = node;
}

declaration_t *find_hashed_symbol(scope_t *scope, bool local_scope_only, usize hash, pstring_t name) {
    if (scope->nodes[hash % 128]) {
        symbol_t *sym = scope->nodes[hash % 128];
        while (sym) {
            if (sym->hash == hash && pcmp_string(sym->name, name))
                return sym->value;
            sym = sym->next;
        }
    }

    if (local_scope_only) return NULL;
    if (!scope->parent) return NULL;
    return find_hashed_symbol(scope->parent, false, hash, name);
}


declaration_t *find_symbol(scope_t *scope, bool local_scope_only, pstring_t name) {
    usize hash = phash(name.c_str, name.length, (usize)hash_seed.value);
    return find_hashed_symbol(scope, local_scope_only, hash, name);
}


typedef struct error_t error_t;
struct error_t {
    bool is_valid;
    pstring_t *stretchy reasons;
};
static const error_t ok = { true, NULL };
static error_t reason_impl(const char *file, usize line, usize column, const char *fmt, va_list list) {
    static const pstream_info_t sstream_info = {
        .type  = STRING_STREAM,
        .flags = STREAM_INPUT|STREAM_OUTPUT,
        .buffersize = 100
    };

    pstring_stream_t sstream = pinit_stream(sstream_info); 
    if (file) {
        pbprintf(&sstream, "%s:%zu:%zu:    ", file, line, column);
    }
    pvbprintf(&sstream, fmt, list);
    pstream_write(&sstream, '\n');

    pstring_t str = pcopy_string(pstream_to_buffer_string(&sstream));
    pstring_t *arr = NULL; psb_pushback(arr, str);
    pfree_stream(&sstream);
    return (error_t){false, arr};
}

static error_t reason_decl(declaration_t *ast, const char *fmt, ...) {
    va_list list;
    va_start(list, fmt);
    error_t e;
    if (!ast)
        e = reason_impl(NULL, 0, 0, fmt, list);
    else {
        token_t begin = ast->begin;
        e = reason_impl(begin.file, begin.line, begin.column, fmt, list);
    }
    va_end(list);
    return e;
}

static error_t reason_stmt(statement_t   *ast, const char *fmt, ...) {
    va_list list;
    va_start(list, fmt);
    error_t e;
    if (!ast)
        e = reason_impl(NULL, 0, 0, fmt, list);
    else {
        token_t begin = ast->begin;
        e = reason_impl(begin.file, begin.line, begin.column, fmt, list);
    }
    va_end(list);
    return e;
}

static error_t reason_expr(expression_t  *ast, const char *fmt, ...) {
    va_list list;
    va_start(list, fmt);
    error_t e;
    if (!ast)
        e = reason_impl(NULL, 0, 0, fmt, list);
    else {
        token_t begin = ast->begin;
        e = reason_impl(begin.file, begin.line, begin.column, fmt, list);
    }
    va_end(list);
    return e;
}

static error_t reason_none(void *, const char *fmt, ...) {
    va_list list;
    va_start(list, fmt);
    error_t e = reason_impl(NULL, 0, 0, fmt, list);
    va_end(list);
    return e;
}

#define reason(ast, ...)       \
    _Generic((ast),                 \
    declaration_t *: reason_decl,   \
    statement_t   *: reason_stmt,   \
    expression_t  *: reason_expr,   \
    void *         : reason_none    \
    )(ast, __VA_ARGS__)

parameter_t *find_parameter_by_name(type_t *func, pstring_t, isize*);

error_t analyse_declaration(typemap_t *, scope_t *, declaration_t *declaration);
error_t analyse_function_declaration(typemap_t *, scope_t *, declaration_t *declaration);
error_t analyse_statement(typemap_t *, scope_t *, type_t *, statement_t *);

error_t analyse_block_statement(typemap_t *, scope_t *, type_t *, statement_t *);
error_t analyse_for_statement(typemap_t *, scope_t *, type_t *, statement_t *);

error_t analyse_expression(scope_t *, expression_t *);
error_t analyse_literal_expression(scope_t *, expression_t *);
error_t analyse_prefix_expression (scope_t *, expression_t *);
error_t analyse_postfix_expression(scope_t *, expression_t *);
error_t analyse_binary_expression (scope_t *, expression_t *);
error_t analyse_assignment_expression(scope_t *, expression_t *);
error_t analyse_comma_expression(scope_t *, expression_t *);
error_t analyse_call_expression(scope_t *, expression_t *);
error_t analyse_subscript_expression(scope_t *, expression_t *);
error_t analyse_member_access_expression(scope_t *, expression_t *);
error_t analyse_lambda_expression(scope_t *, expression_t *);
error_t analyse_cast_expression(scope_t *, expression_t *);
error_t analyse_compound_literal_expression(scope_t *, expression_t *);

bool can_cast(type_t *actual, type_t *cast);
type_t *get_type_from_declaration(declaration_t *);

error_t analyse(usize count, declaration_t *declarations[count]) {
    typemap_t types = yoinky_sploink_types();
    scope_t top_level = {0};

    pstring_t *stretchy reasons = NULL;

    bool has_failed = false;
    for (usize i = 0; i < count; i++) {
        add_declaration_to_scope(&top_level, declarations[i]);
        error_t check = analyse_declaration(&types, &top_level, declarations[i]);
        if (!check.is_valid) {
            has_failed = has_failed || !check.is_valid;
            psb_foreach(check.reasons) psb_pushback(reasons, *it);
        }
    }

    return (error_t){!has_failed, reasons};
}

error_t analyse_declaration(typemap_t *types, scope_t *parent_scope, declaration_t *declaration) {
    error_t err;
    switch (declaration->kind) {
    case declSTRUCTURE: return ok;
    case declOVERLOAD :
    case declFUNCTION : return analyse_function_declaration(types, parent_scope, declaration);
    case declMUTABLE  :
    case declIMMUTABLE:
        if (declaration->expression) {
            if (!(err = analyse_expression(parent_scope, declaration->expression)).is_valid) {
                return err;
            }
            if (!declaration->type) {
                declaration->type = declaration->expression->type;
                return ok;
            }
            
            if (!can_cast(declaration->expression->type, declaration->type)) {
                return reason(declaration->expression, "type mismatch!");
            }
            return ok;
        }

        return declaration->type != NULL ? ok : reason(declaration, "declaration had no type");
    default:
        panic("unexpected declaration variant");
        return reason(declaration, "panicked");
    }
}

error_t analyse_function_declaration(typemap_t *types, scope_t *scope, declaration_t *declaration) {
    error_t err;
    scope_t *function_scope = pallocate(sizeof *function_scope);
    *function_scope = (scope_t){.parent = scope};
    declaration->scope = function_scope;
    //TODO: don't panic on these types of errors, preferrably we store them in a list and print them when exiting

    parameter_list_t *parameters = &declaration->parameters;
    for (usize i = 0; i < parameters->num_unnamed_parameters; i++) {
        parameter_t *param = &parameters->unnamed_parameters[i];
        if (param->value) {
            if (!(err = analyse_expression(function_scope, param->value)).is_valid)
                return err;
            if (!can_cast(param->type, param->value->type))
                return reason(param->value, "type mismatch");
        }
        if (declaration->is_external) continue;
        
        declaration_t decl = {
            .kind = declPARAMETER,
            .type = param->type,
            .identifier = param->name,
            .expression = param->value,
        };
        declaration_t *other;
        if ((other = find_hashed_symbol(function_scope, true, param->hash, param->name)))
            return reason(declaration, "%S was already declared as a parameter", param->name);

        add_declaration_to_scope(function_scope, make_declaration(&decl));
    }
    
    {
        named_parameter_map_t *named_parameters = &parameters->named_parameters;
        usize num_buckets_used = named_parameters->num_used_buckets;
        usize *buckets_with_values = named_parameters->buckets_with_values;
        named_parameter_node_t**nodes = named_parameters->nodes;
        usize num_named_parameters = parameters->num_named_parameters;
        named_parameter_node_t* cur_node = NULL;
        usize bucket_id = 0;
        usize i = 0;

        do {
            if (cur_node == NULL) {
                if (bucket_id == num_buckets_used) break;
                cur_node = nodes[buckets_with_values[bucket_id]];
                bucket_id++;

            }
            //TODO: dry up this code
            parameter_t *param = &cur_node->parameter;
            cur_node = cur_node->next;
            i++;
            if (param->value) {
                if (!(err = analyse_expression(function_scope, param->value)).is_valid)
                    return err;
                if (!can_cast(param->type, param->value->type))
                    return reason(param->value, "type mismatch");
            }
            if (declaration->is_external) continue;
            
            declaration_t decl = {
                .kind = declPARAMETER,
                .type = param->type,
                .identifier = param->name,
                .expression = param->value,
            };
            declaration_t *other;
            if ((other = find_hashed_symbol(function_scope, true, param->hash, param->name)))
                return reason(declaration, "%S was already declared as a parameter", param->name);
            
            add_declaration_to_scope(function_scope, make_declaration(&decl));
        } while (i < num_named_parameters);
    }

    if (declaration->is_external)
        return ok;

    return analyse_statement(types, function_scope, declaration->return_type, declaration->function_body);
}

error_t analyse_statement(typemap_t *types, scope_t *scope, type_t *expected_return_type, statement_t *statement) {
    error_t err;

    switch(statement->kind) {
    case stmtEXPRESSION:
        return analyse_expression(scope, statement->expression);
    case stmtRETURN: {
            if (!(err = analyse_expression(scope, statement->sreturn)).is_valid)
                return err;
            if (!(compare_types(statement->sreturn->type, expected_return_type) || can_cast(statement->sreturn->type, expected_return_type)))
                return reason(statement->sreturn, "type mismatch");
            return ok;
        }
    case stmtBLOCK:
        return analyse_block_statement(types, scope, expected_return_type, statement);
    case stmtFOR:
        return analyse_for_statement(types, scope, expected_return_type, statement);
    case stmtDEFER:
        return analyse_statement(types, scope, expected_return_type, statement->defer);
    case stmtIF:
        if (!(err = analyse_expression(scope, statement->sif.condition)).is_valid)
            return err;
        if (!(err = analyse_statement(types, scope, expected_return_type, statement->sif.true_branch)).is_valid)
            return err;
        if (statement->sif.false_branch && !(err = analyse_statement(types, scope, expected_return_type, statement->sif.false_branch)).is_valid)
            return err;
        return ok;

    case stmtDO: 
    case stmtWHILE:
        if (!(err = analyse_expression(scope, statement->sdo.condition)).is_valid)
            return err;
        return analyse_statement(types, scope, expected_return_type, statement->sdo.block);
    case stmtNOOP: return ok;
    default:
        panic("unexpected statement variant");
        return reason(statement, "panicked!");
    }
}

error_t analyse_block_statement(typemap_t *types, scope_t *scope, type_t *expected_return_type, statement_t *statement) {
    scope_t *block_scope = pallocate(sizeof *block_scope);
    *block_scope = (scope_t){.parent = scope};
    statement->scope = block_scope;

    pstring_t *reasons = NULL;

    bool has_failed = false;
    __auto_type block = &statement->block;
    for (usize i = 0; i < block->num_items; i++) {
        error_t check;
        block_item_t *item = &block->items[i];
        switch (item->kind) {
        case itmDECLARATION:
            check = analyse_declaration(types, block_scope, item->declaration);
            if (check.is_valid) add_declaration_to_scope(block_scope, item->declaration);
            else psb_foreach(check.reasons) psb_pushback(reasons, *it);
            has_failed = has_failed || !check.is_valid;
            continue;
        case itmSTATEMENT:
            check = analyse_statement(types, block_scope, expected_return_type, item->statement);
            if (!check.is_valid) psb_foreach(check.reasons) psb_pushback(reasons, *it);
            has_failed = has_failed || !check.is_valid;
            continue;
        default:
            panic("unexpected item variant");
            return reason(statement, "panicked!");
        }
    }

    return (error_t){ !has_failed, reasons };
}

error_t analyse_for_statement(typemap_t *types, scope_t *scope, type_t *expected_return_type, statement_t *sfor) {
    error_t err;
    scope_t *for_scope = pallocate(sizeof *for_scope);
    *for_scope = (scope_t){.parent = scope};
    sfor->scope = for_scope;

    if (!(err = analyse_declaration(types, scope, sfor->sfor.declaration)).is_valid)
        return err;
    add_declaration_to_scope(for_scope, sfor->sfor.declaration);

    if (!(err = analyse_expression(for_scope, sfor->sfor.condition)).is_valid)
        return err;

    if (!(err = analyse_expression(for_scope, sfor->sfor.increment)).is_valid)
        return err;

    return analyse_statement(types, for_scope, expected_return_type, sfor->sfor.block);
}

error_t analyse_expression(scope_t *scope, expression_t *expression) {
    switch (expression->kind) {
    case exprLITERAL         : return analyse_literal_expression(scope, expression);
    case exprPREFIX          : return analyse_prefix_expression(scope, expression);
    case exprPOSTFIX         : return analyse_postfix_expression(scope, expression);
    case exprBINARY          : return analyse_binary_expression(scope, expression);
    case exprASSIGNMENT      : return analyse_assignment_expression(scope, expression);
    case exprCOMMA           : return analyse_comma_expression(scope, expression);
    case exprCALL            : return analyse_call_expression(scope, expression);
    case exprSUBSCRIPT       : return analyse_subscript_expression(scope, expression);
    case exprPOINTER_MEMBER_ACCESS:
    case exprMEMBER_ACCESS   : return analyse_member_access_expression(scope, expression);
    case exprLAMBDA          : return analyse_lambda_expression(scope, expression);
    case exprCAST            : return analyse_cast_expression(scope, expression);
    case exprCOMPOUND_LITERAL: return analyse_compound_literal_expression(scope, expression);
    default:
            panic("unexpected expression variant");
            return reason(expression, "panicked!");
    }
}


declaration_t *get_overload(usize kind, type_t *lhs, type_t *rhs) {
    if (kind < ovSUBSCRIPT) {
        if (lhs->overloads[kind])
            return lhs->overloads[kind]->function;
        return NULL;
    }

    overload_t *ov = lhs->overloads[kind];
    while (ov) {
        if(compare_types(ov->right, rhs))
            return ov->function;
        ov = ov->next;
    }
    return NULL;
}

//TODO: analyse expression
error_t analyse_literal_expression(scope_t *scope, expression_t *expression) {
    switch (expression->lit_kind) {
    case litIDENTIFIER: {
            declaration_t *decl = find_symbol(scope, false, expression->identifier);
            if (!decl) return reason(expression, "unresolved identifier %S", expression->identifier);
            expression->type = get_type_from_declaration(decl);
            return ok;
        }
    case litSTRING:  expression->type = make_type(get_type(tySTRING, NULL), NULL); expression->type->is_immutable = true; return ok;
    case litFLOAT:   expression->type = make_type(get_type(tyFLOAT , NULL), NULL); expression->type->is_immutable = true; return ok;
    case litUINT:    expression->type = make_type(get_type(tyUINT  , NULL), NULL); expression->type->is_immutable = true; return ok;
    case litINT:     expression->type = make_type(get_type(tyINT   , NULL), NULL); expression->type->is_immutable = true; return ok;
    default:
            panic("unexpected literal expression variant");
            return reason(expression, "panicked!");
    }
}

error_t analyse_prefix_expression(scope_t *scope, expression_t *expression) {
    error_t err;
    if (!(err = analyse_expression(scope, expression->prefix.operand)).is_valid)
        return err;

    type_t *type = expression->type = expression->prefix.operand->type;

    usize kind = 0;
    usize prefix_kind = expression->prefix.kind;
    switch (expression->prefix.kind) {
    default              : return err;
    case exprNOT         : kind = ovNOT;    goto prefix;
    case exprNEGATE      : kind = ovNEGATE; goto prefix;
    case exprSIGN        : kind = ovSIGN;   goto prefix;
    prefix               : err = reason(expression, "unary operator '%c' on function is not a valid expression", prefix_kind);
                           if (!is_overloadable(type)) {
                               while (type->kind == tyREF) {
                                   type = type->ref;
                                   if (!type) break;
                               }
                               if (!type) return reason(expression, "reference to nothing!");
                               switch (type->kind) {
                               case tySTRUCTURE: return err;
                               case tyREF      : return err;
                               case tyVOID     : return err;
                               case tyINT      : return ok;
                               case tyUINT     : return ok;
                               case tyBOOL     : return ok;
                               case tyFLOAT    : return ok;
                               case tySTRING   : return err;
                               case tyARRAY    : return err;
                               case tyPTR      : return ok;
                               case tyNULLABLE : return err;
                               case tyFUNCTION : return err;
                               }
                           }
                           if (type->kind == tyPTR) {
                               if (kind == ovNOT) return ok;
                               return err;
                           }
                           if (type->kind == tyREF) type = type->ref;
                           declaration_t *ov = get_overload(kind, type, NULL);
                           if (ov) {
                               expression->type = ov->return_type;
                               return ok;
                           }
                           switch (type->kind) {
                           case tySTRUCTURE: return err;
                           case tyREF      : return err;
                           case tyVOID     : return err;
                           case tyINT      : return ok;
                           case tyBOOL     : return ok;
                           case tyUINT     : return ok;
                           case tyFLOAT    : return ok;
                           case tySTRING   : return err;
                           case tyARRAY    : return err;
                           case tyPTR      : return ok;
                           case tyNULLABLE : return err;
                           case tyFUNCTION : return err;
                           }
                           return err;
    case exprDEREFERENCE : if (type->kind != tyPTR) {
                               panic("deferencing non pointer type");
                               return err;
                           }
                           expression->type = make_type(type->ptr, &(type_t){ .kind = tyREF });
                           return ok;
    case exprREFERENCE   : expression->type = make_type(type, &(type_t){ .kind = tyPTR });
                           return ok;
    case exprAS_REFERENCE: expression->type = make_type(type, &(type_t){ .kind = tyREF });
                           return ok;
    }
}

error_t analyse_postfix_expression(scope_t *scope, expression_t *expression) {
    error_t err;
     if (!(err = analyse_expression(scope, expression->postfix.operand)).is_valid)
        return err;

    type_t *type = expression->type = expression->postfix.operand->type;

    if (type->kind != tyNULLABLE)
        return reason(expression, "null operator on nonnullable type!");

    switch (expression->postfix.kind) {
    default           : return reason(expression, "unexpected postfix expression kind");
    case exprUNWRAP   : expression->type = type->nullable;
                        return ok;
    case exprNULLCHECK: return ok;
    }
    return ok;
}

error_t analyse_binary_expression(scope_t *scope, expression_t *expression) {
    error_t err;
    if (!(err = analyse_expression(scope, expression->binary.left)).is_valid)
        return err;
    if (!(err = analyse_expression(scope, expression->binary.right)).is_valid)
        return err;

    usize kind;
    __auto_type binary = &expression->binary;
    __auto_type left   = binary->left ;
    __auto_type right  = binary->right;
    __auto_type left_type = left ? left->type : NULL;

    if (!right->type) 
        return reason(expression, "binary operator with void type");

    switch (binary->kind) {
    default             : return reason(expression, "unexpected binary expression kind");
    case NULL_COALESCING: 
        if (!left->type || left_type->kind != tyNULLABLE)
            return reason(expression, "null coalescing on nonnullable type");
        return ok;
    case BOOLEAN_OR :
    case BOOLEAN_AND:
        if (!can_cast(left->type, get_type(tyBOOL, NULL)))
            return reason(expression, "boolean && on type not convertible to bool");
        if (!can_cast(right->type, get_type(tyBOOL, NULL)))
            return reason(expression, "boolean && on type not convertible to bool");
        return ok;
    case EQUAL_EQUAL       : kind = ovEQUALITY;         goto binary;
    case GREATER_THAN_EQUAL: kind = ovGREATERTHANEQUAL; goto binary;
    case LESS_THAN_EQUAL   : kind = ovLESSTHANEQUAL;    goto binary;
    case '<'               : kind = ovLESSTHAN;         goto binary;
    case '>'               : kind = ovGREATERTHAN;      goto binary;
    case '+'               : kind = ovADDITION;         goto binary;
    case '-'               : kind = ovSUBTRACT;         goto binary;
    case '*'               : kind = ovMULTIPLY;         goto binary;
    case '/'               : kind = ovDIVIDE;           goto binary;
    case '%'               : kind = ovMODULO;           goto binary;
    binary                 :
        err = reason(expression, "binary operator '%c' is not a valid expression", binary->kind);
        if (!is_overloadable(binary->left->type)) {
            while (left_type->kind == tyREF) {
                left_type = left_type->ref;
                if (!left_type) break;
            }
            if (!left_type) return reason(expression, "reference to nothing!");
            switch (left_type->kind) {
            case tyREF      : return err;
            case tyVOID     : return err;
            case tyNULLABLE : return err;
            case tyFUNCTION : return err;
            case tySTRUCTURE: return err;
            case tySTRING   : return err;
            case tyARRAY    : {
                    if (kind == ovDIVIDE || kind == ovMODULO || kind == ovMULTIPLY)
                        return err;

                    type_t *right_type = right->type;
                    while (right_type->kind == tyREF) right_type = right_type->ref;

                    switch (right_type->kind) {
                        case tyARRAY    : return err;
                        case tyNULLABLE : return err;
                        case tyFUNCTION : return err;
                        case tySTRUCTURE: return err;
                        case tySTRING   : return err;
                        case tyREF      : return err;
                        case tyVOID     : return err;
                        case tyFLOAT    : return err;
                        case tyINT      : 
                        case tyUINT     : 
                        case tyBOOL     : expression->type = make_type( left_type->arr.type, &(type_t){.kind = tyPTR});
                                          return ok;
                        case tyPTR      : if (compare_types(left_type->arr.type, right_type->ptr))
                                              expression->type = right_type;
                                          else expression->type = make_type(get_type(tyVOID, NULL), &(type_t){.kind = tyPTR});
                                          return ok;
                    }
                    return err;
                }
            case tyINT      :
            case tyUINT     :
            case tyBOOL     :
            case tyFLOAT    : {
                    type_t *right_type = right->type;
                    while (right_type->kind == tyREF) right_type = right_type->ref;

                    expression->type = left_type;
                    switch (right_type->kind) {
                        case tyARRAY    : return err;
                        case tyNULLABLE : return err;
                        case tyFUNCTION : return err;
                        case tySTRUCTURE: return err;
                        case tySTRING   : return err;
                        case tyREF      : return err;
                        case tyVOID     : return err;
                        case tyPTR      : return err;
                        case tyINT      : return ok;
                        case tyUINT     : return ok;
                        case tyBOOL     : return ok;
                        case tyFLOAT    : return ok;
                    }
                    return err;
                }
            case tyPTR      : {
                    if (kind == ovDIVIDE || kind == ovMODULO || kind == ovMULTIPLY)
                        return err;
                    type_t *right_type = right->type;
                    while (right_type->kind == tyREF) right_type = right_type->ref;

                    type_t *ptr_type = NULL;
                    switch (right_type->kind) {
                        case tyNULLABLE : return err;
                        case tyFUNCTION : return err;
                        case tySTRUCTURE: return err;
                        case tySTRING   : return err;
                        case tyREF      : return err;
                        case tyVOID     : return err;
                        case tyFLOAT    : return err;
                        case tyINT      : return ok;
                        case tyUINT     : return ok;
                        case tyBOOL     : return ok;
                        case tyARRAY    : ptr_type = left_type->arr.type; goto lbl0_pointer;
                        case tyPTR      : ptr_type = left->type->ptr;     goto lbl0_pointer;
                        lbl0_pointer    :
                                          if (compare_types(ptr_type, right_type->ptr))
                                              expression->type = right_type;
                                          else expression->type = make_type(get_type(tyVOID, NULL), &(type_t){.kind = tyPTR});
                                          return ok;
                    }
                    return err;
                }
            }
        }
        if (left_type->kind == tyPTR) {
            return ok;
        }
        if (left_type->kind == tyREF) left_type = left_type->ref;
        declaration_t *ov = get_overload(kind, left_type, right->type);
        if (ov) {
            expression->type = ov->return_type;
            return ok;
        }
        switch (left_type->kind) {
        case tyREF      : return err;
        case tyVOID     : return err;
        case tyNULLABLE : return err;
        case tyFUNCTION : return err;
        case tySTRUCTURE: return err;
        case tySTRING   : return err;
        case tyARRAY    : {
                if (kind == ovDIVIDE || kind == ovMODULO || kind == ovMULTIPLY)
                    return err;

                type_t *right_type = right->type;
                while (right_type->kind == tyREF) right_type = right_type->ref;

                switch (right_type->kind) {
                    case tyARRAY    : return err;
                    case tyNULLABLE : return err;
                    case tyFUNCTION : return err;
                    case tySTRUCTURE: return err;
                    case tySTRING   : return err;
                    case tyREF      : return err;
                    case tyVOID     : return err;
                    case tyFLOAT    : return err;
                    case tyINT      : 
                    case tyUINT     : 
                    case tyBOOL     : expression->type = make_type( left_type->arr.type, &(type_t){.kind = tyPTR});
                                      return ok;
                    case tyPTR      : if (compare_types(left_type->arr.type, right_type->ptr))
                                          expression->type = right_type;
                                      else expression->type = make_type(get_type(tyVOID, NULL), &(type_t){.kind = tyPTR});
                                      return ok;
                }
                return err;
            }
        case tyINT      :
        case tyUINT     :
        case tyBOOL     :
        case tyFLOAT    : {
                type_t *right_type = right->type;
                while (right_type->kind == tyREF) right_type = right_type->ref;

                expression->type = left_type;
                switch (right_type->kind) {
                    case tyARRAY    : return err;
                    case tyNULLABLE : return err;
                    case tyFUNCTION : return err;
                    case tySTRUCTURE: return err;
                    case tySTRING   : return err;
                    case tyREF      : return err;
                    case tyVOID     : return err;
                    case tyPTR      : return err;
                    case tyINT      : return ok;
                    case tyUINT     : return ok;
                    case tyBOOL     : return ok;
                    case tyFLOAT    : return ok;
                }
                return err;
            }
        case tyPTR      : {
                if (kind == ovDIVIDE || kind == ovMODULO || kind == ovMULTIPLY)
                    return err;
                type_t *right_type = right->type;
                while (right_type->kind == tyREF) right_type = right_type->ref;

                type_t *ptr_type = NULL;
                switch (right_type->kind) {
                    case tyNULLABLE : return err;
                    case tyFUNCTION : return err;
                    case tySTRUCTURE: return err;
                    case tySTRING   : return err;
                    case tyREF      : return err;
                    case tyVOID     : return err;
                    case tyFLOAT    : return err;
                    case tyINT      : return ok;
                    case tyUINT     : return ok;
                    case tyBOOL     : return ok;
                    case tyARRAY    : ptr_type = left_type->arr.type; goto lbl1_pointer;
                    case tyPTR      : ptr_type = left->type->ptr;     goto lbl1_pointer;
                    lbl1_pointer    :
                                      if (compare_types(ptr_type, right_type->ptr))
                                          expression->type = right_type;
                                      else expression->type = make_type(get_type(tyVOID, NULL), &(type_t){.kind = tyPTR});
                                      return ok;
                }
                return err;
            }
        }
        return err;
    }
    return reason(expression, "%s unreachable state reached", __func__);
}

error_t analyse_assignment_expression(scope_t *scope, expression_t *expression) {
    error_t err;
    if (!(err = analyse_expression(scope, expression->obj)).is_valid)
        return err;
    
    if (!(err = analyse_expression(scope, expression->expression)).is_valid)
        return err;

    type_t *lhs = expression->obj->type;
    type_t *rhs = expression->expression->type;
    expression->type = lhs;

    while (lhs->kind == tyREF) lhs = lhs->ref;
    while (rhs->kind == tyREF) rhs = rhs->ref;
    if (!can_cast(rhs, lhs))
        return reason(expression, "typemismatch in assignment");

    if (expression->type->is_immutable)
        return reason(expression, "assign on immutable variable");
    return ok;
}

error_t analyse_comma_expression(scope_t *scope, expression_t *expression) {
    error_t err;
    (void)scope;(void)expression;
    panic("yay comma expression!");
    return err;
}

struct required_parameter_t {
        bool filled;
        parameter_t *param;
} *gather_required_parameters(type_t *type, usize *result_count) {
    usize num_required = 0;
    struct required_parameter_t *required_parameters = NULL;
    
    parameter_list_t *parameters = &type->parameters;
    bool nullable = true;
    if (type->parameters.num_unnamed_parameters) {
        usize num_unnamed_required = 0;
        for (isize i = parameters->num_unnamed_parameters - 1; i >= 0; i--) {
            parameter_t *param = &parameters->unnamed_parameters[i];
            type_t *ty = param->type;
            if (!ty) {
                pfree(required_parameters);
                *result_count = 0;
                return NULL;
            }
            if (ty->kind == tyNULLABLE && nullable) continue;
            if (ty->kind != tyNULLABLE) nullable = false;
            num_unnamed_required++;
        }
        for (usize i = 0; i < num_unnamed_required; i++) {
            parameter_t *param = &parameters->unnamed_parameters[i];
            struct required_parameter_t result = { false, param };
            psb_pseudo_pushback(required_parameters, num_required, result);
        }
    }
    {
        named_parameter_map_t *named_parameters = &parameters->named_parameters;
        usize num_buckets_used = named_parameters->num_used_buckets;
        usize *buckets_with_values = named_parameters->buckets_with_values;
        named_parameter_node_t**nodes = named_parameters->nodes;
        usize num_named_parameters = parameters->num_named_parameters;
        named_parameter_node_t* cur_node = NULL;
        usize bucket_id = 0;
        usize i = 0;

        do {
            if (cur_node == NULL) {
                if (bucket_id == num_buckets_used) break;
                cur_node = nodes[buckets_with_values[bucket_id]];
                bucket_id++;
            }
            //TODO: dry up this code
            parameter_t *param = &cur_node->parameter;
            cur_node = cur_node->next;
            i++;

            type_t *ty = param->type;
            if (!ty) {
                pfree(required_parameters);
                *result_count = 0;
                return NULL;
            }
            if (ty->kind == tyNULLABLE && nullable) continue;

            struct required_parameter_t result = { false, param };
            psb_pseudo_pushback(required_parameters, num_required, result);

        } while (i < num_named_parameters);
    }

    *result_count = num_required;
    return required_parameters;
}

error_t analyse_call_expression(scope_t *scope, expression_t *expression) {
    error_t err;
    if (!(err = analyse_expression(scope, expression->obj)).is_valid)
        return err;

    if (expression->obj->type->kind != tyFUNCTION) return err;

    type_t *func = expression->obj->type;
    usize num_parameters = func->parameters.num_parameters;
    if (expression->num_parameters > num_parameters)
        return reason(expression, "too many parameters");

    usize num_required = 0;//TODO: fix this leak
    struct required_parameter_t *required_parameters = gather_required_parameters(func, &num_required);

    if (required_parameters == NULL) {
        usize unnamed_index = 0;
        for (usize i = 0; i < expression->num_parameters; i++) {
            struct call_parameter_t *param = &expression->parameters[i];
            if (!(err = analyse_expression(scope, param->value)).is_valid)
                return err;

            if (param->named) {
                isize index;
                parameter_t *parameter = find_parameter_by_name(func, param->name, &index);
                if (!parameter) return reason(expression, "parameter '%S' is not a valid parameter", param->name);
                if (!can_cast(param->value->type, parameter->type))
                    return reason(expression, "type mismatch in parameters");
                if (index != -1) unnamed_index = index;
                continue;
            }
            if (unnamed_index >= func->parameters.num_unnamed_parameters)
                return reason(expression, "too many unnamed parameters for function call", param->name);

            parameter_t *parameter = &func->parameters.unnamed_parameters[unnamed_index++];
            if (!can_cast(param->value->type, parameter->type))
                return reason(expression, "type mismatch in parameters");
            continue;
        }
    } else {
        usize unnamed_index = 0;
        for (usize i = 0; i < expression->num_parameters; i++) {
            struct call_parameter_t *param = &expression->parameters[i];
            if (!(err = analyse_expression(scope, param->value)).is_valid)
                return err;

            if (param->named) {
                isize index;
                parameter_t *parameter = find_parameter_by_name(func, param->name, &index);
                if (!parameter) return reason(expression, "parameter '%S' is not a valid parameter", param->name);
                if (!can_cast(param->value->type, parameter->type))
                    return reason(expression, "type mismatch in parameters");

                for (usize i = 0; i < num_required; i++) {
                    if (parameter == required_parameters[i].param) {
                        if (required_parameters[i].filled)
                            return reason(expression, "parameter was filled more than once");
                        required_parameters[i].filled = true;
                    }
                }
                if (index != -1) unnamed_index = index;
                continue;
            }
            if (unnamed_index >= func->parameters.num_unnamed_parameters)
                return reason(expression, "too many unnamed parameters for function call", param->name);

            parameter_t *parameter = &func->parameters.unnamed_parameters[unnamed_index++];
            for (usize i = 0; i < num_required; i++) {
                if (parameter == required_parameters[i].param) {
                    if (required_parameters[i].filled)
                        return reason(expression, "parameter was filled more than once");
                    required_parameters[i].filled = true;
                }
            }
            if (!can_cast(param->value->type, parameter->type))
                return reason(expression, "type mismatch in parameters");
            continue;
        }

        for (usize i = 0; i < num_required; i++)
            if (!required_parameters[i].filled)
                return reason(expression, "required parameter '%S' has no value", required_parameters[i].param->name);
    }

    expression->type = func->return_type;
    return ok;
}

error_t analyse_subscript_expression(scope_t *scope, expression_t *expression) {
    error_t err;
    if (!(err = analyse_expression(scope, expression->obj)).is_valid)
        return err;
    if (!(err = analyse_expression(scope, expression->index)).is_valid)
        return err;

    type_t *obj_type = expression->obj->type;
    while (obj_type->kind == tyREF) obj_type = obj_type->ref;
    
    type_t *index_type = expression->index->type;
    while (index_type->kind == tyREF) index_type = index_type->ref;

    if ((obj_type->kind == tyARRAY || obj_type->kind == tyPTR) &&
            (index_type->kind == tyINT || index_type->kind == tyUINT || index_type->kind == tyBOOL))
    {
        expression->type = make_type(obj_type->arr.type, &(type_t){ .kind = tyREF });
        return ok;
    }

    declaration_t *overload = get_overload(ovSUBSCRIPT, obj_type, index_type);
    if (!overload) return reason(expression, "no subscript overload found");
    expression->type = overload->return_type;
    return ok;
}

error_t analyse_member_access_expression(scope_t *scope, expression_t *expression) {
    error_t err;
    
    if (!(err = analyse_expression(scope, expression->member_access_obj)).is_valid)
        return err;


    type_t *type = expression->member_access_obj->type;
    while (type->kind == tyREF) type = type->ref;

    if (expression->kind == exprPOINTER_MEMBER_ACCESS) {
        if (type->kind != tyPTR) return reason(expression, "pointer member access on non pointer type!");
        type = type->ptr;
    }

    if (type->kind != tySTRUCTURE) return reason(expression, "member access on non struct type");
    if (expression->is_integral) return reason(expression, "integral member access is currently not supported");

    for (usize i = 0; i < type->num_fields; i++) 
        if (pcmp_string(expression->member, type->fields[i].name)) {
            expression->type = type->fields[i].type;
            return ok;
        }
    return reason(expression, "no member named '%S' in struct", expression->member);
}

error_t analyse_cast_expression(scope_t *scope, expression_t *expression) {
    error_t err;
    if (!(err = analyse_expression(scope, expression->cast_expression)).is_valid)
        return err;

    type_t *type = expression->cast_expression->type;
    if (!can_cast(type, expression->cast_type))
        return reason(expression, "type mismatch");
    
    expression->type = expression->cast_type;
    return ok;
}
error_t analyse_lambda_expression(scope_t *, expression_t *e) { return reason(e, "not implemented %s", __func__); }

usize get_expected_compound_value_count(type_t *type) {
    switch (type->kind) {
    case tyFUNCTION : return 0;
    case tyVOID     : return 0;
    case tyINT      : return 1;
    case tyUINT     : return 1;
    case tyBOOL     : return 1;
    case tyFLOAT    : return 1;
    case tySTRING   : return 1;
    case tyARRAY    : return type->arr.is_dynamic ? -1 : type->arr.size;
    case tyREF      : return 1;
    case tyPTR      : return 1;
    case tyNULLABLE : return 1;
    case tySTRUCTURE: return type->num_fields;
    }
}

type_t *get_expected_compound_type(type_t *type, usize index) {
    switch (type->kind) {
    case tyFUNCTION : return NULL;
    case tyVOID     : return NULL;
    case tyINT      : return type;
    case tyUINT     : return type;
    case tyBOOL     : return type;
    case tyFLOAT    : return type;
    case tySTRING   : return type;
    case tyARRAY    : return type->arr.type;
    case tyREF      : return type->ref;
    case tyPTR      : return type->ptr;
    case tyNULLABLE : return type->nullable;
    case tySTRUCTURE: return type->fields[index].type;
    }
}

error_t analyse_compound_literal_expression(scope_t *scope, expression_t *expression) {
    error_t err;

    __auto_type compound = &expression->compound;
    type_t *type = compound->type;
    if (type->kind == tyFUNCTION || type->kind == tyVOID)
        return reason(expression, "cannot make a compound literal for function or void types");
    
    usize expected_num_values = get_expected_compound_value_count(type);

    if (compound->num_values > expected_num_values)
        return reason(expression, "too many values in compound expression");

    for (usize i = 0; i < compound->num_values; i++) {
        type_t *field_type = get_expected_compound_type(type, i);
        if (!(err = analyse_expression(scope, compound->values[i])).is_valid)
            return err;
        if (!can_cast(compound->values[i]->type, field_type))
            return reason(expression, "type mismatch");
    }

    expression->type = compound->type;
    return ok;
}

parameter_t *find_named_parameter_hashed(named_parameter_map_t *map, usize hash, pstring_t name) {
    if (!map->nodes[hash % 128]) return NULL;
    
    named_parameter_node_t *node = map->nodes[hash % 128];
    do {
        if (node->parameter.hash != hash) { 
            node = node->next;
            continue;
        }
        
        if (pcmp_string(name, node->parameter.name))
            return &node->parameter;
        node = node->next;
    } while (node);
    return NULL;
}

parameter_t *find_named_parameter(named_parameter_map_t *map, pstring_t name) {
    usize hash = phash(name.c_str, name.length, hash_seed.value);
    return find_named_parameter_hashed(map, hash, name);
}

bool can_cast(type_t *actual, type_t *cast) {
    if (!(actual && cast)) return false;
    if (cast->kind == tyREF) return compare_types(actual, cast->ref);

    if (cast->kind == tyNULLABLE)
        return can_cast(actual, cast->nullable);

    switch (actual->kind) {
    case tyVOID     : return cast->kind == tyVOID;
    case tyBOOL     :
    case tyINT      : 
    case tyUINT     : if (cast->kind == tyPTR) return true;
    case tyFLOAT    : 
        switch (cast->kind) {
        case tyINT  : 
        case tyUINT : 
        case tyFLOAT: return true;
        default     : return false;
        }
    case tySTRING   : return compare_types(actual, cast);
    case tyARRAY    : if (cast->kind == tyPTR && compare_types(actual->arr.type, cast->ptr))
                          return true;
                      return false;
    case tyREF      : return can_cast(actual->ref, cast);
    case tyPTR      : if (cast->kind == tyPTR || cast->kind == tyINT || cast->kind == tyUINT)
                          return true;
                      return false;
    case tyFUNCTION : if (cast->kind == tyPTR)
    case tyNULLABLE :   return compare_types(actual, cast); 
    case tySTRUCTURE: {
            declaration_t *overload = get_overload(ovUSERCAST, actual, cast);
            if (overload) return true;

            // check if these types are equivalent
            // they are equivalent if their fields are the same (not considering private/public)
            if (cast->kind != tySTRUCTURE) return false;
            if (actual->num_fields != cast->num_fields) return false;
            for (usize i = 0; i < actual->num_fields; i++) 
                if (!compare_types(actual->fields[i].type, actual->fields[i].type))
                    return false;
            return true;
        }
    default: return false;
    }
}

bool compare_types(type_t *left, type_t * right) {
    if (!(left && right)) return false;

    if (left->kind == tyREF && compare_types(left->ref, right))
        return true;

    if (left->kind != right->kind) return false;

    switch (left->kind) {
    case tyVOID     :
    case tyINT      : 
    case tyUINT     : 
    case tyFLOAT    :
    case tySTRING   : return true; 
    case tyARRAY    : {
            if (!compare_types(left->arr.type, right->arr.type))
                return false;
            
            if (left->arr.is_dynamic || right->arr.is_dynamic)
                return true;
            return left->arr.size == right->arr.size;
        }
    case tyREF      : return compare_types(left->ref, right->ref);
    case tyPTR      : return true;
    case tyNULLABLE : return compare_types(left->nullable, right->nullable);
    case tyFUNCTION : {
            if (!compare_types(left->return_type, right->return_type))
                return false;

            __auto_type params_left  = &left->parameters;
            __auto_type params_right = &right->parameters;
            if (params_left->num_parameters != params_right->num_parameters)
                return false;
            
            if (params_left->num_unnamed_parameters != params_right->num_unnamed_parameters)
                return false;
            
            if (params_left->num_named_parameters != params_right->num_named_parameters)
                return false;

            for (usize i = 0; i < params_left->num_unnamed_parameters; i++)
                if (!compare_types(params_left->unnamed_parameters[i].type, params_right->unnamed_parameters[i].type))
                    return false;

            {
                named_parameter_map_t *named_parameters = &params_left->named_parameters;
                usize num_buckets_used = named_parameters->num_used_buckets;
                usize *buckets_with_values = named_parameters->buckets_with_values;
                named_parameter_node_t**nodes = named_parameters->nodes;
                usize num_named_parameters = params_left->num_named_parameters;
                named_parameter_node_t* cur_node = NULL;
                usize bucket_id = 0;
                usize i = 0;

                do {
                    if (cur_node == NULL) {
                        if (bucket_id == num_buckets_used) break;
                        cur_node = nodes[buckets_with_values[bucket_id]];
                        bucket_id++;
                    }
                    parameter_t *left = &cur_node->parameter;
                    parameter_t *right = find_named_parameter_hashed(&params_right->named_parameters, left->hash, left->name);
                    if (!right) return false;
                    if (!compare_types(left->type, right->type)) return false;

                    cur_node = cur_node->next;

                    i++;
                } while (i < num_named_parameters);
            }
            return true;
        }
    case tySTRUCTURE: {
            if (left->num_fields != right->num_fields)
                return false;
            for (usize i = 0; i < left->num_fields; i++) 
                if (!compare_types(left->fields[i].type, right->fields[i].type))
                    return false;
            return true;
        }
    default: return false;
    }
}

type_t *get_type_from_declaration(declaration_t *declaration) {
    switch (declaration->kind) {
    case declSTRUCTURE: return make_type(&declaration->structure, NULL);
    case declOVERLOAD : 
    case declFUNCTION : {
            return make_type(&(type_t){
                .kind = tyFUNCTION,
                .parameters = declaration->parameters,
                .return_type = declaration->return_type,
                .is_immutable = true
            }, NULL);
        }
    case declMUTABLE  : 
    case declIMMUTABLE: 
    case declPARAMETER: 
             type_t *result = declaration->type ? make_type(declaration->type, NULL) : make_type(get_type(tyVOID, NULL), NULL);
             if (declaration->kind == declIMMUTABLE)
                 result->is_immutable = true;
             return result;
    default: return NULL;
    }
}

parameter_t *find_parameter_by_name(type_t *func, pstring_t name, isize *unnamed_index) {
    usize hash = phash(name.c_str, name.length, hash_seed.value);
    
    *unnamed_index = -1;
    parameter_list_t *parameters = &func->parameters;
    {
        named_parameter_map_t *named_parameters = &parameters->named_parameters;
        usize num_buckets_used = named_parameters->num_used_buckets;
        usize *buckets_with_values = named_parameters->buckets_with_values;
        named_parameter_node_t**nodes = named_parameters->nodes;
        usize num_named_parameters = parameters->num_named_parameters;
        named_parameter_node_t* cur_node = NULL;
        usize bucket_id = 0;
        usize i = 0;

        do {
            if (cur_node == NULL) {
                if (bucket_id == num_buckets_used) break;
                cur_node = nodes[buckets_with_values[bucket_id]];
                bucket_id++;
            }
            //TODO: dry up this code
            parameter_t *param = &cur_node->parameter;
            cur_node = cur_node->next;
            i++;
            
            if (param->hash != hash) continue;
            if (!pcmp_string(param->name, name)) 
                continue;

            return param;
        } while (i < num_named_parameters);
    }
    for (isize i = 0; i < parameters->num_unnamed_parameters; i++) {
        parameter_t *param = &parameters->unnamed_parameters[i];
        
        if (param->hash != hash) continue;
        if (!pcmp_string(param->name, name)) 
            continue;
    
        *unnamed_index = i;
        return param;
    }
    return NULL;
}
