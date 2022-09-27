#include "parser.h"
#include "stretchy_buffer.h"
#include "hash.h"

typemap_t type_map = {
    .base_types = {
        [tyVOID  ] = { .kind = tyVOID  },
        [tyINT   ] = { .kind = tyINT   },
        [tyUINT  ] = { .kind = tyUINT  },
        [tyBOOL  ] = { .kind = tyBOOL  },
        [tyFLOAT ] = { .kind = tyFLOAT },
        [tySTRING] = { .kind = tySTRING},
    }
};

union hash_value_t hash_seed = {.buf = "language"};
overload_t *make_overload(type_t *rhs, declaration_t *decl, overload_t *next);

typemap_t yoinky_sploink_types(void) {
    return type_map;
}

void push_user_type(pstring_t name, type_t *type) {
    usize hash = phash(name.c_str, name.length, (usize)hash_seed.value);
    node_t *node = pallocate(sizeof *node);
    node->hash  = hash;
    node->value = type;
    node->name  = name;
    node->next  = NULL;

    if (!type_map.nodes[hash % 128]) {
        type_map.nodes[hash % 128] = node;
        return;
    }

    node_t *list = type_map.nodes[hash % 128];
    while (list->next) list = list->next;
    list->next = node;
}

type_t *tm_find_user_type(typemap_t *map, pstring_t name) {
    usize hash = phash(name.c_str, name.length, (usize)hash_seed.value);
    if (!map->nodes[hash % 128])
        return NULL;
    node_t *list = map->nodes[hash % 128];
    do {
        if (list->hash == hash && pcmp_string(list->name, name))
            return list->value;
    } while (list);
    return NULL;
}

type_t *find_user_type(pstring_t name) {
    return tm_find_user_type(&type_map, name);
}

type_t *get_type(usize kind, pstring_t *name) {
    switch (kind) {
    case tyVOID     : return &type_map.base_types[kind];
    case tyINT      : return &type_map.base_types[kind];
    case tyUINT     : return &type_map.base_types[kind];
    case tyBOOL     : return &type_map.base_types[kind];
    case tyFLOAT    : return &type_map.base_types[kind];
    case tySTRING   : return &type_map.base_types[kind];
    case tySTRUCTURE: passert(name); return find_user_type(*name);
    default         : return NULL;
    }
}

void add_parameter(bool is_named, parameter_list_t *decl, pstring_t name, type_t *type, expression_t *value) {
    usize hash = phash(name.c_str, name.length, (usize)hash_seed.value);
    parameter_t parameter = {
        .hash = hash,
        .name = name,
        .type = type,
        .value = value
    };

    if (is_named) {
        usize node_index = hash % 128;

        struct named_parameter_node_t *node = pallocate(sizeof *node);
        node->next = NULL;
        node->parameter = parameter;

        __auto_type named_parameters = &decl->named_parameters; 
        if (!named_parameters->nodes[node_index]) {
            named_parameters->buckets_with_values[named_parameters->num_used_buckets++] = node_index;
            named_parameters->nodes[node_index] = node;
            decl->num_named_parameters++;
            decl->num_parameters++;
            return;
        }

        struct named_parameter_node_t *list = named_parameters->nodes[node_index];
        while (list->next) list = list->next;

        list->next = node;
        decl->num_named_parameters++;
        decl->num_parameters++;
        return;
    }

    psb_pseudo_pushback(decl->unnamed_parameters, decl->num_unnamed_parameters, parameter);
    decl->num_parameters++;
}

parameter_list_t parse_parameter_list(void) {
    token_t token = get_current_token();
    if (is_not(&token, '(')) return (parameter_list_t){0};

    parameter_list_t list = {0};
    consume_current_token();
    bool named_parameters = false;
    while (is_not((token = get_current_token(),&token), ')')) {
        if (!named_parameters && is(&token, '{')) {
            named_parameters = true;
            consume_current_token();
            continue;
        }
    
        token = get_current_token();
        if (is_not(&token, IDENTIFIER))
            panic("expected an identifier!");
        
        pstring_t identifier = token.identifier;
        token = consume_current_token();
        
        if (is_not(&token, ':'))
            panic("expected an ':'!");
    
        consume_current_token();
        type_t *type = parse_typename();
        
        expression_t *expression = NULL;
        token = get_current_token();
        if (is(&token, '=')) {
            token = consume_current_token();
            expression = parse_expression();
        }
        
        add_parameter(named_parameters, &list, identifier, type, expression);
    
        if (named_parameters && is(&token, '}')) {
            consume_current_token();
            break;
        }
    
        if (is(&token, ')')) break;
        expect_and_consume(',');
    }
    expect_and_consume(')');

    return list;
}

declaration_t *parse_structure_definition(void) {
    token_t token = get_current_token();
    if (is_not(&token, STRUCT)) return NULL;
    token_t first = token;
    consume_current_token();

    token_t typename = get_current_token();
    expect_and_consume(IDENTIFIER);
    
    value_t *stretchy values = NULL;
    field_t *stretchy fields = NULL;
    expect_and_consume('{');
    while (is_not(&token, '}')) {
        token = get_current_token();
        if (is_not(&token, IDENTIFIER))
            panic("expected an identifier!");
        
        pstring_t identifier = token.identifier;
        consume_current_token();
        
        type_t *type = NULL;
        token = get_current_token();
        if (is(&token, ':')) {
            consume_current_token();
            type = parse_typename();
        }
        
        value_t value = {0};
        token = get_current_token();
        if (is(&token, '=')) {
            token = consume_current_token();
            //TODO: fix memory leak
            expression_t *expr = parse_and_fold_expression();
            passert(expr && expr->is_constant);
            //TODO: make a convert constant function
            value = expr->value;
        }

        field_t field = { 
            .is_private = identifier.c_str[0] == '_',
            .type       = type,
            .name       = identifier
        };
        psb_pushback(fields, field);
        psb_pushback(values, value);
        
        token = get_current_token();
        if (is(&token, '}')) break;
        //TODONE: make expect_and_consume return current token
        token = expect_and_consume(';');
    }
    token_t last = get_current_token();
    expect_and_consume('}');

    declaration_t structure = {
        .kind = declSTRUCTURE,
        .begin = first, .end = last,
        .structure = {
            .kind = tySTRUCTURE,
            .name       = typename.identifier,
            .num_fields = psb_size(fields)
        }
    };
    structure.structure.fields = psb_unstretch(fields);
    structure.structure.default_value.structure = psb_unstretch(values);

    declaration_t *decl = make_declaration(&structure);
    push_user_type(typename.identifier, &decl->structure); 
    return decl;
}

declaration_t *parse_function_declaration(void) {
    token_t token = get_current_token();
    token_t first = token;
    bool is_external = is(&token, EXTERN);
    if (is_external) token = consume_current_token();

    if (is_not(&token, FN)) {
        if (is_external) panic("keyword 'extern' not followed by 'fn'");
        return NULL;
    }
    consume_current_token();

    token = get_current_token();

    token_t operator = token;
    pstring_t identifier = token.identifier;
    bool has_binary  = true;
    bool is_operator = false;
    switch((usize)token.kind) {
    case '['               : consume_current_token();
                             expect_and_consume(']');
                             is_operator = true;
                             break;
    case AS                : has_binary = false;
    case '-'               :
    case '+'               :
    case '*'               :
    case '/'               :
    case '%'               :
    case EQUAL_EQUAL       :
    case GREATER_THAN_EQUAL:
    case LESS_THAN_EQUAL   :
    case '>'               :
    case '<'               : is_operator = true;
                             token = consume_current_token();
                             break;
    default                : if (is_not(&token, IDENTIFIER))
                                 panic("expected an identifier!");
                             token = consume_current_token();
    }

    declaration_t decl = {
        .begin = first,
        .kind = declFUNCTION,
        .name = identifier,
    };

    decl.parameters = parse_parameter_list();
    
    token = get_current_token();
    if (is(&token, ':')) {
        consume_current_token();
        decl.return_type = parse_typename();
    }


    decl.end = get_current_token();
    if (is_external) {
        decl.is_external = is_external;
        expect_and_consume(';');
    } else decl.function_body = parse_statement();

    if (is_operator) {
        usize max_param = has_binary ? 2 : 1;
        if (decl.parameters.num_parameters > max_param)
            panic("operator overload has too many parameters");
        
        if (decl.parameters.num_named_parameters > 0)
            panic("operator overload has named parameters");

        usize kind;
        switch((usize)operator.kind) {
        case '['               : kind = ovSIGN; break;
        case AS                : kind = ovUSERCAST;
                                 if (!decl.return_type || decl.return_type->kind == tyVOID)
                                     panic("usercast to void is not supported");
                                 break;
        case '!'               : kind = ovNOT; break;
        case '-'               : 
                                 if (decl.parameters.num_parameters == 1) 
                                      kind = ovNEGATE;
                                 else kind = ovSUBTRACT;
                                 break;
        case '+'               : 
                                 if (decl.parameters.num_parameters == 1) 
                                      kind = ovSIGN;
                                 else kind = ovADDITION;
                                 break;
        case '*'               : kind = ovMULTIPLY; break;
        case '/'               : kind = ovDIVIDE; break;
        case '%'               : kind = ovMODULO; break;
        case EQUAL_EQUAL       : kind = ovEQUALITY; break;
        case GREATER_THAN_EQUAL: kind = ovGREATERTHANEQUAL; break;
        case LESS_THAN_EQUAL   : kind = ovLESSTHANEQUAL; break;
        case '>'               : kind = ovLESSTHAN; break;
        case '<'               : kind = ovGREATERTHAN; break;
        }

        type_t *lhs = decl.parameters.unnamed_parameters[0].type;
        type_t *rhs = decl.parameters.num_parameters == 2 ? 
            decl.parameters.unnamed_parameters[1].type : NULL;

        if (!is_overloadable(lhs))
            panic("overload on non user type");

        if (lhs->kind == tyREF || lhs->kind == tyPTR)
            lhs = lhs->ref;

        decl.kind = declOVERLOAD;
        overload_t *ov = make_overload(rhs, make_declaration(&decl), lhs->overloads[kind]);
        lhs->overloads[kind] = ov;
        return ov->function;
    }
    return make_declaration(&decl);
}

statement_t *parse_statement(void) {
    token_t token = get_current_token();
    token_t first = token;
    
    if (is(&token, ';')) {
        token = consume_current_token();
        return make_statement(&(statement_t){.kind=stmtNOOP});
    }

    if (is(&token, '{')) {
        token = consume_current_token();
        
        usize count = 0;
        block_item_t *items = NULL;
        while (is_not(((token = get_current_token()), &token), '}')) {
            block_item_t item;

            declaration_t *decl = parse_declaration();
            if (decl) {
                item.kind = itmDECLARATION;
                item.declaration = decl;
            } else {
                item.kind = itmSTATEMENT;
                item.statement = parse_statement();
                if (!item.statement) {
                    token = consume_current_token();
                    panic("%s:%llu:%llu:expected statement or declaration!", token.file, token.line, token.column);
                }
            }
            psb_pseudo_pushback(items, count, item);
        }

        token_t last = get_current_token();
        expect_and_consume('}');
        return make_statement(&(statement_t){
            .begin = first, .end = last,
            .kind  = stmtBLOCK,
            .block = { count, items }
        });
    }

    if (is(&token, IF)) {
        token = consume_current_token();

        expression_t *expression = parse_expression();
        token_t last = get_current_token();

        statement_t  *statement  = parse_statement();
        statement_t result = {
            .begin = first, .end = last,
            .kind = stmtIF, 
            .sif = { .condition = expression, .true_branch = statement }
        };
        token = get_current_token();
        if (is(&token, ELSE)) {
            token_t last = get_current_token();
            consume_current_token();
            result.end = last,
            result.sif.false_branch = parse_statement();
        }

        return make_statement(&result);
    }
    if (is(&token, FOR)) {
        bool has_parenthesis = false;
        token = consume_current_token();
        if (is(&token, '(')) {
            has_parenthesis = true;
            token = consume_current_token();
        }
        
        declaration_t *declaration = parse_declaration();
        expression_t  *condition   = parse_expression();
        expect_and_consume(';');
        expression_t  *iteration   = parse_expression();
        token = consume_current_token();
        token_t last = get_current_token();
        if(has_parenthesis) expect_and_consume(')');
        return make_statement(&(statement_t){
            .kind   = stmtFOR, 
            .begin = first, .end = last,
            .sfor = { 
                declaration, 
                condition, 
                iteration,
                parse_statement()
            }
        });
    }
    if (is(&token, WHILE)) {
        token = consume_current_token();
        expression_t *expression = parse_expression();
        token_t last = get_current_token();

        statement_t *statement = parse_statement();
        return make_statement(&(statement_t){
            .kind   = stmtWHILE, 
            .begin = first, .end = last,
            .swhile = { expression, statement }
        });
    }
    if (is(&token, DO)) {
        token = consume_current_token();
        statement_t *statement = parse_statement();

        expect_and_consume(WHILE);
        expression_t *expression = parse_expression();
        token_t last = get_current_token();
        expect_and_consume(';');

        return make_statement(&(statement_t){
            .kind = stmtDO, 
            .begin = first, .end = last,
            .sdo = { expression, statement }
        });
    }
    if (is(&token, DEFER)) {
        token = consume_current_token();
        bool is_block = is(&token, '{');
        statement_t *result = make_statement(&(statement_t){
            .begin = first,
            .kind = stmtDEFER, 
            .defer = parse_statement()
        });
        result->end = get_current_token();
        if (!is_block) expect_and_consume(';');
        return result;
    }
    if (is(&token, RETURN)) {
        token = consume_current_token();
        statement_t *result = make_statement(&(statement_t){
            .begin = first,
            .kind = stmtRETURN,
            .sreturn = parse_expression()
        });
        result->end = get_current_token();
        expect_and_consume(';');
        return result;
    }

    expression_t *expression = parse_expression();
    if (!expression) return NULL;
    token_t last = get_current_token();
    expect_and_consume(';');

    return make_statement(&(statement_t){
        .begin = first, .end = last,
        .kind = stmtEXPRESSION, 
        .expression = expression
    });
}

declaration_t *parse_declaration(void) {
    bool is_mutable = false;
    token_t token = get_current_token();

    //TODO: function declaration
    if (is(&token, EXTERN) || is(&token, FN))
        return parse_function_declaration();
    if (is(&token, STRUCT))
        return parse_structure_definition();
    if (is_not(&token, LET)) return NULL;
    token_t first = token;
    consume_current_token();
    
    token = get_current_token();
    if (is(&token, MUT)) {
        is_mutable = true;
        consume_current_token();
    }

    token = get_current_token();
    if (is_not(&token, IDENTIFIER))
        panic("expected an identifier!");

    pstring_t identifier = token.identifier;
    token = consume_current_token();

    type_t *type = NULL;
    if (is(&token, ':')) {
        consume_current_token();
        type = parse_typename();
    }

    expression_t *expression = NULL;
    token = get_current_token();
    if (is(&token, '=')) {
        token = consume_current_token();
        expression = parse_expression();
    }

    token_t last = get_current_token();
    expect_and_consume(';');

    return make_declaration(&(declaration_t) {
        .kind = is_mutable ? declMUTABLE : declIMMUTABLE,
        .begin = first, .end = last,
        .identifier = identifier,
        .type       = type,
        .expression = expression
    });
}



type_t *parse_typename(void) {
    type_t *type = NULL;
    //TODONE: create a table of typenames so that we can look up identifiers
    //      currently they are just ignored, making user types unusable

    token_t first = get_current_token();
repeat:
    token_t token = get_current_token();
    if (is(&token, '*')) {
        type = make_type(&(type_t){ .kind = tyPTR }, type);
        consume_current_token();
        goto repeat;
    }
    if (is(&token, '?')) {
        type = make_type(&(type_t){ .kind = tyNULLABLE }, type);
        consume_current_token();
        goto repeat;
    }
    if (is(&token, REF)) {
        type = make_type(&(type_t){ .kind = tyREF }, type);
        consume_current_token();
        goto repeat;
    }
    if (is(&token, '[')) {
        consume_current_token();
        type_t arr = {.kind = tyARRAY};
        if (is(&token, '*')) arr.arr.is_dynamic = true;
        else {
            expression_t *expr = parse_and_fold_expression();
            passert(expr && expr->is_constant);
            passert(expr->lit_kind == litINT || expr->lit_kind == litUINT);
            arr.arr.size = (expr->lit_kind == litINT) ? (usize)expr->value.ival : expr->value.uval;
        }
        type = make_type(&arr, type);
        expect_and_consume(']');
        goto repeat;
    }
    if (is(&token, INT   )) { consume_current_token(); return make_type(get_type(tyINT   , NULL), type); }
    if (is(&token, UINT  )) { consume_current_token(); return make_type(get_type(tyUINT  , NULL), type); }
    if (is(&token, FLOAT )) { consume_current_token(); return make_type(get_type(tyFLOAT , NULL), type); }
    if (is(&token, VOID  )) { consume_current_token(); return make_type(get_type(tyVOID  , NULL), type); }
    if (is(&token, STRING)) { consume_current_token(); return make_type(get_type(tySTRING, NULL), type); }
    if (is(&token, BOOL  )) { consume_current_token(); return make_type(get_type(tyBOOL  , NULL), type); }
    if (is(&token, IDENTIFIER)) {
        type_t *structure = get_type(tySTRUCTURE, &token.identifier);
        if (!structure) return NULL;
        consume_current_token(); 
        return make_type(structure, type);
    }
    if (is(&token, FN)) {
        token = consume_current_token();
        type_t func = {
            .kind = tyFUNCTION,
            .parameters = parse_parameter_list()
        };

        token = get_current_token();
        if (is(&token, ':')) {
            consume_current_token();
            func.return_type = parse_typename();
        }
        return make_type(&func, type);
    }
    return NULL;
}

type_t *make_type(type_t *type_stub, type_t *prev_type) {
    type_t *type = NULL;
    if (type_stub->kind == tySTRUCTURE)
        type = type_stub;
    else {
        type = pallocate(sizeof *type);
        memcpy(type, type_stub, sizeof *type);
    }

    if (prev_type) {
        //TODO: actually spit out an error here
        passert(prev_type->kind == tyARRAY || 
               prev_type->kind == tyREF    || 
               prev_type->kind == tyPTR    ||
               prev_type->kind == tyNULLABLE);

        type_t *result = prev_type;
        while (prev_type->ptr) prev_type = prev_type->ptr;
        prev_type->ptr = type;
        return result;
    }
    return type;
}

expression_t *parse_and_fold_expression(void) {
    return parse_expression();
}

expression_t *parse_expression(void) {
    //TODO: parsing comma expressions
    return parse_assignment_expression();
}

expression_t *parse_non_comma_expression(void) {
    return parse_assignment_expression();
}

expression_t *parse_assignment_expression(void) {
    token_t first = get_current_token();
    expression_t *expr = parse_inquisitive_expression();

    while (true) {
        token_t token = get_current_token();
        usize operator = 0;
        switch ((usize)token.kind) {
        default: 
            return expr;
        case '=':
                consume_current_token();
                expr = make_expression(&(expression_t){
                    .kind = exprASSIGNMENT,
                    .obj  = expr, .expression = parse_inquisitive_expression()
                });
                token_t last = get_current_token();
                expr->begin = first;
                expr->end   = last;
                continue;
        case ADD_EQUAL         : operator = '+';             goto to_assignment;
        case SUB_EQUAL         : operator = '-';             goto to_assignment;
        case MUL_EQUAL         : operator = '*';             goto to_assignment;
        case DIV_EQUAL         : operator = '/';             goto to_assignment;
        case MOD_EQUAL         : operator = '%';             goto to_assignment;
        case NULL_ASSIGN       : operator = NULL_COALESCING; goto to_assignment;
            {
        to_assignment:
                token_t first_right = get_current_token();
                consume_current_token();
                expression_t *right = parse_inquisitive_expression();
                right = make_expression(&(expression_t){
                        .kind = exprBINARY, 
                        .binary = { operator, expr, right }
                });
        
                expr = make_expression(&(expression_t){
                    .kind = exprASSIGNMENT,
                    .obj  = expr, .expression = right
                });
                token_t last = get_current_token();
                right->begin = first_right;
                expr->begin  = first;
                expr->end    = last;
                continue;
            }
        } 
    }
}

bool is_overloadable(type_t *ty) {
    if (!ty) return false;

    if(ty->kind == tyPTR || ty->kind == tyREF) {
        if (!ty->ref) return false;
        ty = ty->ref;
    }

    switch (ty->kind) {
    case tyVOID     :
    case tyFUNCTION : 
    case tyNULLABLE : 
    case tyREF      : 
    case tyPTR      :
    case tyARRAY    :
    default         : return false;
    case tyINT      : 
    case tyUINT     : 
    case tyFLOAT    :
    case tySTRING   : 
    case tySTRUCTURE: return true;
    }
}

expression_t *parse_inquisitive_expression(void) {
    token_t first = get_current_token();
    expression_t *expr = parse_equality_expression();
    
    while (true) {
        token_t token = get_current_token();
        switch ((usize)token.kind) {
        default: return expr;
        case AS:
            consume_current_token();
            expr = make_expression(&(expression_t){
                .kind            = exprCAST,
                .cast_expression = expr,
                .cast_type       = parse_typename(),
            });
            expr->begin = first;
            expr->end   = get_current_token();
            break;
        case NULL_COALESCING:
        case BOOLEAN_OR:
        case BOOLEAN_AND:
            consume_current_token();
            expr = make_expression(&(expression_t){
                .kind = exprBINARY,
                .binary = { token.kind, expr, parse_additive_expression() }
            });
            expr->begin = first;
            expr->end   = get_current_token();
        }
    }
}

expression_t *parse_equality_expression(void) {
    token_t first = get_current_token();
    expression_t *expr = parse_additive_expression();

    while (true) {
        token_t token = get_current_token();
        switch ((usize)token.kind) {
        default: return expr;
        case EQUAL_EQUAL:
        case GREATER_THAN_EQUAL:
        case LESS_THAN_EQUAL:
        case '<':
        case '>':
            consume_current_token();
            expr = make_expression(&(expression_t){
                .kind = exprBINARY,
                .binary = { token.kind, expr, parse_additive_expression() }
            });
            expr->begin = first;
            expr->end   = get_current_token();
        }
    }
}

expression_t *parse_additive_expression(void) {
    token_t first = get_current_token();
    expression_t *expr = parse_multiplicative_expression();

    while (true) {
        token_t token = get_current_token();
        switch ((usize)token.kind) {
        default: return expr;
        case '+': 
        case '-': 
            consume_current_token();
            expr = make_expression(&(expression_t){
                .kind = exprBINARY,
                .binary = { token.kind, expr, parse_multiplicative_expression() }
            });
            expr->begin = first;
            expr->end   = get_current_token();
        }
    }
}

expression_t *parse_multiplicative_expression(void) {
    token_t first = get_current_token();
    expression_t *expr = parse_prefix_expression();

    while (true) {
        token_t token = get_current_token();
        switch ((usize)token.kind) {
        default: return expr;
        case '*': 
        case '/': 
        case '%': 
            consume_current_token();
            expr = make_expression(&(expression_t){
                .kind = exprBINARY,
                .binary = { token.kind, expr, parse_prefix_expression() }
            });
            expr->begin = first;
            expr->end   = get_current_token();
        }
    }
}

expression_t *parse_prefix_expression(void) {
    token_t first = get_current_token();
    token_t token = get_current_token();

    usize operator = token.kind;
    switch ((usize)token.kind) {
    default : return parse_postfix_expression();
    case '!': consume_current_token(); operator = token.kind; break;
    case '-': consume_current_token(); operator = token.kind; break;
    case '+': consume_current_token(); operator = token.kind; break;
    case '*': consume_current_token(); operator = token.kind; break;
    case '&': consume_current_token(); operator = token.kind; break;
    case REF: consume_current_token(); operator = token.kind; break;
    }

    return make_expression(&(expression_t){
        .kind     = exprPREFIX,
        .begin = first,
        .end   = get_current_token(),
        .prefix   = { operator, parse_prefix_expression() }
    });
}

expression_t *parse_postfix_expression(void) {
    token_t first = get_current_token();
    expression_t *expr = parse_primary_expression();

    while (true) {
        token_t token = get_current_token();

        switch ((usize)token.kind) {
        default: return expr;
        case '!':
        case '?':
            consume_current_token();
            expr = make_expression(&(expression_t){
                .kind     = exprPOSTFIX,
                .postfix = { token.kind, expr }
            });
            expr->begin = first;
            expr->end   = get_current_token();
            continue;
        case ARROW:
        case '.': {
                bool by_pointer = token.kind == ARROW;
                token = consume_current_token();
                if (!(is(&token, LITERAL_UINT) || is(&token, IDENTIFIER))) {
                    panic("expected identifier or literal int");
                }
                consume_current_token();
                
                expression_t access = {
                    .kind = by_pointer ? exprPOINTER_MEMBER_ACCESS : exprMEMBER_ACCESS,
                    .member_access_obj = expr,
                    .is_integral = is(&token, LITERAL_UINT)
                };
                
                if (access.is_integral)
                    access.uval   = token.uval;
                else access.member = token.identifier;
                
                expr = make_expression(&access);
                expr->begin = first;
                expr->end   = get_current_token();
                continue;
            }
        case '[': {
                consume_current_token();
                
                expr = make_expression(&(expression_t){
                        .kind = exprSUBSCRIPT,
                        .subscript_obj = expr,
                        .index = parse_expression()
                        });
                
                expr->begin = first;
                expr->end   = get_current_token();
                expect_and_consume(']');
                continue;
            }
        case '(': {
                consume_current_token();
                expression_t call = {
                    .kind = exprCALL,
                    .called_obj = expr
                };
                while (is_not((token = get_current_token(), &token), ')')) {
                    struct call_parameter_t param = {0};
                
                    token_t tok = peek_next_token();
                    if (is(&token, IDENTIFIER) && is(&tok, ':')) {
                        param.named = true;
                        param.name = token.identifier;
                        expect_and_consume(IDENTIFIER);
                        expect_and_consume(':');
                    }
                
                    param.value = parse_non_comma_expression();
                    psb_pseudo_pushback(call.parameters, call.num_parameters, param);
                    token = get_current_token();
                    if (is(&token, ')')) break;
                    expect_and_consume(',');
                }
                expect_and_consume(')');
                expr = make_expression(&call);
                expr->begin = first;
                expr->end   = get_current_token();
                continue;
            }
        }
    }
}

expression_t *parse_primary_expression(void) {
    token_t token = get_current_token();
    token_t start = token;

    //TODONE: sub expression
    //TODO: lambdas
    
    if (is(&token, '(')) {
        consume_current_token();
        expression_t *expr = parse_expression();
        expect_and_consume(')');
        return expr;
    }

    value_t value;
    usize lit_kind;
    switch ((usize)token.kind) {
    default: return NULL;
    case '.'           : {
            consume_current_token();
            type_t *type = parse_typename();
            if (!type) panic("expected typename!");
            token = expect_and_consume('{');
            usize count = 0;
            expression_t **expressions = NULL;
            bool is_constant = true;

            while (is_not((token = get_current_token(), &token), ')')) {
                expression_t *expression = parse_non_comma_expression();
                if (!expression->is_constant)
                    is_constant = false;
                psb_pseudo_pushback(expressions, count, expression);
                token = get_current_token();
                if (is(&token, '}')) break;
                expect_and_consume(',');
            }
            token_t end = get_current_token();
            expect_and_consume('}');

            return make_expression(&(expression_t){
                .kind = exprCOMPOUND_LITERAL,
                .begin = start, .end = end,
                .is_constant = is_constant,
                .compound = { type, count, expressions }
            });
        }
    case IDENTIFIER    :
        consume_current_token();
        return make_expression(&(expression_t){
            .kind = exprLITERAL,
            .begin = start, .end = start,
            .is_constant = true,
            .lit_kind = litIDENTIFIER,
            .identifier = token.identifier,
        });
    case LITERAL_INT   : lit_kind = litINT   ; value.ival = token.ival; goto create_expression; 
    case LITERAL_UINT  : lit_kind = litUINT  ; value.uval = token.uval; goto create_expression; 
    case LITERAL_FLOAT : lit_kind = litFLOAT ; value.fval = token.fval; goto create_expression; 
    case LITERAL_STRING: lit_kind = litSTRING; value.sval = token.sval; goto create_expression; 
    case LITERAL_TRUE  : lit_kind = litBOOL  ; value.bval = true;       goto create_expression;
    case LITERAL_FALSE : lit_kind = litBOOL  ; value.bval = false;      goto create_expression;
    create_expression: 
        consume_current_token();
        return make_expression(&(expression_t){
            .kind = exprLITERAL,
            .begin = start, .end = start,
            .is_constant = true,
            .lit_kind    = lit_kind,
            .value       = value,
        });
    }
}

overload_t *make_overload(type_t *rhs, declaration_t *decl, overload_t *next) {
    overload_t *ov = pallocate(sizeof *ov);
    *ov = (overload_t){0};
    ov->right = rhs;
    ov->function = decl;
    ov->next = next;
    return ov;
}

expression_t *make_expression(expression_t *new) {
    expression_t *expr = pallocate(sizeof *expr);
    *expr = *new;
    return expr;
}

declaration_t *make_declaration(declaration_t *new) {
    declaration_t *decl = pallocate(sizeof *decl);
    *decl = *new;
    return decl;
}

statement_t *make_statement(statement_t *new) {
    statement_t *stmt = pallocate(sizeof *stmt);
    *stmt = *new;
    return stmt;
}




