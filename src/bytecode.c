
#include "bytecode.h"
#include "hash.h"

void *salloc(stack_allocator_t *stack, usize size) {
    if (!stack) return NULL;

    usize pos = stack->stack_pointer;
    usize ptr = stack->stack_pointer + size;
    if (ptr >= stack->end_of_storage)
        return NULL;

    return stack->data + pos;
}

data_t *get_data(vm_t *vm, usize index) {
    data_table_t *table = &vm->data;
    while (index > 255) {
        index -= 255;
        table = table->next;
    }
    return table->slots[index];
}

function_t vm_find_function(vm_t *, pstring_t);
vm_t btc_init_vm(module_t *mod, usize stack_size) {
    return (vm_t){
        .stack.data    = pallocate(stack_size),
        .num_bytecodes = mod->num_bytecodes,
        .bytecodes     = mod->bytecodes,
        .functions     = mod->functions
    };
}

bool vm_run(vm_t *vm, function_t fn) {
    if (fn.is_external)
        return fn.external(vm, 0, NULL);
    if (vm->num_bytecodes == 0) return true;

    while (vm->pc < vm->num_bytecodes) {
        bytecode_t code = vm->bytecodes[vm->pc++];
        switch (code.mnemonic) {
        case btcNOOP: break;
        case btcPUSH: {
                vm->sbp += code.dst.index;
                salloc(&vm->stack, code.dst.index);
                break;
            }
        case btcPOP : {
                vm->sbp -= code.dst.index;
                break;
            }
        case btcDATA: {
                usize slot   = code.dst.index;
                usize offset = code.offset;
                slot %= 255;

                data_table_t *table = &vm->data;
                while (table->slots_in_use == 255) {
                    if (!table->next) {
                        table->next = pzero_allocate(sizeof *table);
                        table = table->next;
                        break;
                    } else table = table->next;
                }

                table->slots[slot] = (void*)&vm->stack.data[vm->sbp - offset];
                break;
            }
             
             // MOVE:
        case btcCNUL: panic("btcCNUL bytecode not implemented");
        case btcNMOV: panic("btcNMOV bytecode not implemented");
        case btcNUFS: panic("btcNUFS bytecode not implemented");
        case btcINFS: panic("btcINFS bytecode not implemented");
        case btcUNFS: panic("btcUNFS bytecode not implemented");
        case btcFNFS: panic("btcFNFS bytecode not implemented");
        case btcUMOV: {
                data_t *dst = get_data(vm, code.dst.index);
                dst->uval = code.src.index;
                break;
            }
        case btcIMOV: {
                data_t *dst = get_data(vm, code.dst.index);
                dst->ival = (data_t){.uval = code.src.index}.ival;
                break;
            }
        case btcFMOV: {
                data_t *dst = get_data(vm, code.dst.index);
                dst->ival = (data_t){.uval = code.src.index}.fval;
                break;
            }

        case btcVMOV: {
                data_t *dst = get_data(vm, code.dst.index);
                data_t *src = get_data(vm, code.src.index);
                memmove(dst, src, code.size);
                break;
            }
        case btcRMOV: {
                data_t *dst = get_data(vm, code.dst.index);
                data_t *src = get_data(vm, code.src.index);
                dst->pval = &src->uval;
                break;
            }
        case btcDMOV: {
                data_t *dst = get_data(vm, code.dst.index);
                data_t *src = get_data(vm, code.src.index);
                memmove(dst, src->pval, code.size);
                break;
            }
             
             // CAST:
        case btcICNF: panic("btcICNF bytecode not implemented");
        case btcFCNI: panic("btcFCNI bytecode not implemented");
                             
             // FUNCTION / JUMPS
        case btcCALL: panic("btcCALL bytecode not implemented");
        case btcRET : panic("btcRET  bytecode not implemented");
        case btcJUMP: panic("btcJUMP bytecode not implemented");
        case btcJIFT: panic("btcJIFT bytecode not implemented");
        case btcJIFN: panic("btcJIFN bytecode not implemented");

             // MATH
        #define BINARY(val, op, ...) {                                          \
                data_t *dst  = get_data(vm, code.dst.index);                    \
                data_t *srca = get_data(vm, code.srca.index);                   \
                data_t *srcb = get_data(vm, code.srcb.index);                   \
                dst->PSTD_DEFAULT(__VA_ARGS__, val) = (srca->val op srcb->val); \
                break;                                                          \
            }
        #define UNARY(val, op, ...) {                                \
                data_t *dst = get_data(vm, code.dst.index);          \
                data_t *src = get_data(vm, code.src.index);          \
                dst->PSTD_DEFAULT(__VA_ARGS__, val) = (op src->val); \
                break;                                               \
            }
        case btcIEQU: BINARY(ival, ==, uval)
        case btcIGTE: BINARY(ival, >=, uval)
        case btcILTE: BINARY(ival, <=, uval)
        case btcIGET: BINARY(ival, > , uval)
        case btcILET: BINARY(ival, < , uval)
        case btcIOR:  BINARY(ival, ||, uval)
        case btcIAND: BINARY(ival, &&, uval)
        case btcIADD: BINARY(ival, + )
        case btcISUB: BINARY(ival, - )
        case btcIMUL: BINARY(ival, * )
        case btcIDIV: BINARY(ival, / )
        case btcIMOD: BINARY(ival, % )
        case btcINOT: UNARY (ival, ! , uval)
        case btcINEG: UNARY (ival, - )
        case btcISGN: UNARY (ival, + )
        
        case btcUEQU: BINARY(uval, ==, uval)
        case btcUGTE: BINARY(uval, >=, uval)
        case btcULTE: BINARY(uval, <=, uval)
        case btcUGET: BINARY(uval, > , uval)
        case btcULET: BINARY(uval, < , uval)
        case btcUOR : BINARY(uval, ||, uval)
        case btcUAND: BINARY(uval, &&, uval)
        case btcUADD: BINARY(uval, + )
        case btcUSUB: BINARY(uval, - )
        case btcUMUL: BINARY(uval, * )
        case btcUDIV: BINARY(uval, / )
        case btcUMOD: BINARY(uval, % )
        case btcUNOT: UNARY (uval, ! , uval)
        case btcUNEG: UNARY (uval, - )
        case btcUSGN: UNARY (uval, + )

        case btcFEQU: BINARY(uval, ==, uval)
        case btcFGTE: BINARY(uval, >=, uval)
        case btcFLTE: BINARY(uval, <=, uval)
        case btcFGET: BINARY(uval, > , uval)
        case btcFLET: BINARY(uval, < , uval)
        case btcFADD: BINARY(uval, + )
        case btcFSUB: BINARY(uval, - )
        case btcFMUL: BINARY(uval, * )
        case btcFDIV: BINARY(uval, / )
        case btcFNOT: UNARY (uval, ! , uval)
        case btcFNEG: UNARY (uval, - )
        case btcFSGN: UNARY (uval, + )
        }
    }

    return true;
}

struct { 
    usize id;
    module_t mod;
} bytecode_builder;

void btc_begin(void) { bytecode_builder.id = 0; bytecode_builder.mod = (module_t){0}; }
module_t btc_end(usize num_bytecodes, bytecode_t bytecodes[num_bytecodes]) {
    module_t mod = bytecode_builder.mod;
    mod.num_bytecodes = num_bytecodes;
    mod.bytecodes = bytecodes;
    return mod;
}

bytecode_t btc_noop(void) { bytecode_builder.id++; return (bytecode_t){.mnemonic= btcNOOP}; }

#undef btc_label
bytecode_t btc_label(label_t *lbl) {
    lbl->index = bytecode_builder.id++;
    return (bytecode_t){.mnemonic= btcNOOP};
}

bytecode_t btc_function(function_t *fn, pstring_t name) {
    *fn = (function_t){0};
    fn->hash = phash(name.c_str, name.length, hash_seed.value),
    fn->name = name;
    fn->index = bytecode_builder.id++;
    {
        function_table_t *table = &bytecode_builder.mod.functions;
        usize num_functions = table->count;
        while (num_functions >= 255) {
            if (!table->next) {
                passert(num_functions - 255 < 255);
                table->next = pzero_allocate(sizeof *table);
                table = table->next;
                break;
            }

            table = table->next;
            num_functions -= 255;
        }
        table->functions[num_functions] = *fn;
    }
    return (bytecode_t){.mnemonic= btcNOOP};
}

bytecode_t btc_push(usize size) { bytecode_builder.id++; return (bytecode_t){btcPUSH, .value = size}; }
bytecode_t btc_pop (usize size) { bytecode_builder.id++; return (bytecode_t){btcPOP , .value = size}; }

bytecode_t btc_data(register_t *reg, usize index, usize offset) {
    bytecode_builder.id++;
    *reg = (register_t){ index };
    return (bytecode_t){btcDATA, .dst1=*reg, offset};
}
                                                           
// MOVE                                            
bytecode_t btc_nmov(register_t dst, register_t src, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcNMOV, .dst4 = dst, src, size};
}

bytecode_t btc_nufs(register_t dst, register_t src, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcNUFS, .dst4 = dst, src, size};
}

bytecode_t btc_infs(register_t dst, usize value) {
    bytecode_builder.id++;
    return (bytecode_t){btcNUFS, .dst0 = dst, .literal.uval= value};
}

bytecode_t btc_unfs(register_t dst, isize value) {
    bytecode_builder.id++;
    return (bytecode_t){btcUNFS, .dst0 = dst, .literal.ival= value};
}

bytecode_t btc_fnfs(register_t dst, f64   value) {
    bytecode_builder.id++;
    return (bytecode_t){btcFNFS, .dst0 = dst, .literal.fval= value};
}

bytecode_t btc_umov(register_t dst, usize value) {
    bytecode_builder.id++;
    return (bytecode_t){btcUMOV, .dst0 = dst, .literal.uval= value};
}

bytecode_t btc_imov(register_t dst, isize value) {
    bytecode_builder.id++;
    return (bytecode_t){btcIMOV, .dst0 = dst, .literal.ival= value};
}

bytecode_t btc_fmov(register_t dst, f64   value) {
    bytecode_builder.id++;
    return (bytecode_t){btcFMOV, .dst0 = dst, .literal.fval= value};
}

bytecode_t btc_vmov(register_t dst, register_t src, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcVMOV, .dst4 = dst, src, size};
}

bytecode_t btc_rmov(register_t dst, register_t src, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcRMOV, .dst4 = dst, src, size};
}

bytecode_t btc_dmov(register_t dst, register_t src, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcDMOV, .dst4 = dst, src, size};
}

// CASTS
bytecode_t btc_icnf(register_t, register_t);    // icnf, dst, src ; conver
bytecode_t btc_fcni(register_t, register_t);    // fcni, dst, src ; conver                    
                                                   
// FUNCTION / JUMPS         
bytecode_t btc_call(register_t, function_t, usize, register_t[]);    // call, dst, func, [args.                   
bytecode_t btc_ret(register_t);    // ret   src                                 
bytecode_t btc_jump(label_t);    // jump, lbl                                 
bytecode_t btc_jift(label_t, register_t);    // jift, lbl, src  ; jump                    
bytecode_t btc_jifn(label_t, register_t);    // jifn, lbl, src  ; jump                    

// MATH
bytecode_t btc_cnul(register_t dst, register_t src) {
    bytecode_builder.id++;
    return (bytecode_t){btcCNUL, .dst4 = dst, src, 0};
}

// SIGNED MATH
bytecode_t btc_iequ(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcIEQU, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_igte(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcIGTE, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_ilte(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcILTE, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_iget(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcIGET, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_ilet(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcILET, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_ior (register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcIOR, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_iand(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcIAND, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_iadd(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcIADD, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_isub(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcISUB, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_imul(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcIMUL, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_idiv(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcIDIV, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_imod(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcIMOD, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_inot(register_t dst, register_t src, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcINOT, .dst4= dst, src, size};
}

bytecode_t btc_ineg(register_t dst, register_t src, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcINEG, .dst4= dst, src, size};
}

bytecode_t btc_isgn(register_t dst, register_t src, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcISGN, .dst4= dst, src, size};
}


// UNSIGNED MATH
bytecode_t btc_uequ(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcUEQU, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_ugte(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcUGTE, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_ulte(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcULTE, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_uget(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcUGET, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_ulet(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcULET, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_uor (register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcUOR, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_uand(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcUAND, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_uadd(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcUADD, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_usub(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcUSUB, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_umul(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcUMUL, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_udiv(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcUDIV, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_umod(register_t dst, register_t srca, register_t srcb, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcUMOD, .dst3= dst, srca, srcb, size};
}

bytecode_t btc_unot(register_t dst, register_t src, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcUNOT, .dst4= dst, src, size};
}

bytecode_t btc_uneg(register_t dst, register_t src, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcUNEG, .dst4= dst, src, size};
}

bytecode_t btc_usgn(register_t dst, register_t src, usize size) {
    bytecode_builder.id++;
    return (bytecode_t){btcUSGN, .dst4= dst, src, size};
}


// FLOATING POINT MATH
bytecode_t btc_fequ(register_t dst, register_t srca, register_t srcb) {
    bytecode_builder.id++;
    return (bytecode_t){btcFEQU, .dst3= dst, srca, srcb, 0};
}

bytecode_t btc_fgte(register_t dst, register_t srca, register_t srcb) {
    bytecode_builder.id++;
    return (bytecode_t){btcFGTE, .dst3= dst, srca, srcb, 0};
}

bytecode_t btc_flte(register_t dst, register_t srca, register_t srcb) {
    bytecode_builder.id++;
    return (bytecode_t){btcFLTE, .dst3= dst, srca, srcb, 0};
}

bytecode_t btc_fget(register_t dst, register_t srca, register_t srcb) {
    bytecode_builder.id++;
    return (bytecode_t){btcFGET, .dst3= dst, srca, srcb, 0};
}

bytecode_t btc_flet(register_t dst, register_t srca, register_t srcb) {
    bytecode_builder.id++;
    return (bytecode_t){btcFLET, .dst3= dst, srca, srcb, 0};
}

bytecode_t btc_fadd(register_t dst, register_t srca, register_t srcb) {
    bytecode_builder.id++;
    return (bytecode_t){btcFADD, .dst3= dst, srca, srcb, 0};
}

bytecode_t btc_fsub(register_t dst, register_t srca, register_t srcb) {
    bytecode_builder.id++;
    return (bytecode_t){btcFSUB, .dst3= dst, srca, srcb, 0};
}

bytecode_t btc_fmul(register_t dst, register_t srca, register_t srcb) {
    bytecode_builder.id++;
    return (bytecode_t){btcFMUL, .dst3= dst, srca, srcb, 0};
}

bytecode_t btc_fdiv(register_t dst, register_t srca, register_t srcb) {
    bytecode_builder.id++;
    return (bytecode_t){btcFDIV, .dst3= dst, srca, srcb, 0};
}

bytecode_t btc_fnot(register_t dst, register_t src) {
    bytecode_builder.id++;
    return (bytecode_t){btcFNOT, .dst4= dst, src, 0};
}

bytecode_t btc_fneg(register_t dst, register_t src) {
    bytecode_builder.id++;
    return (bytecode_t){btcFNEG, .dst4= dst, src, 0};
}

bytecode_t btc_fsgn(register_t dst, register_t src) {
    bytecode_builder.id++;
    return (bytecode_t){btcFSGN, .dst4= dst, src, 0};
}

