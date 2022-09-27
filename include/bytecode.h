
#include "general.h"
#include "parser.h"

#define btcFUNCTION (btcFSGN + 1)

typedef union data_t data_t;
union data_t { 
    usize uval;
    isize ival;
    f64   fval;
    void *pval;
};
typedef struct register_t register_t;
struct register_t {
    usize index;
};
typedef struct bytecode_t bytecode_t;
struct bytecode_t {
    enum : u8 {
        btcNOOP,    
        btcPUSH,    // push, N        ; push N bytes onto the stack
        btcPOP ,    // pop , N        ; pop  N bytes off  the stack
        btcDATA,    // data 'id' is stack data at address (sbp + soff)
                                                           
        // MOVE                                            
        btcNMOV,    // nmov, dst, src ; move i
        btcNUFS,    // nufs, dst, src ; move if null
        btcINFS,    // infs, dst, lit ; move literal unsigned int if null
        btcUNFS,    // unfs, dst, lit ; move literal signed int   if null
        btcFNFS,    // fnfs, dst, lit ; move literal float        if null
        btcUMOV,    // umov, dst, lit ; move literal unsigned int 
        btcIMOV,    // imov, dst, lit ; move literal signed int 
        btcFMOV,    // fmov, dst, lit ; move literal float       
        btcVMOV,    // vmov, dst, src ; value 
        btcRMOV,    // rmov, dst, src ; move a
        btcDMOV,    // dmov, dst, src ; from v                   
                                                           
        // CASTS
        btcICNF,    // icnf, dst, src ; conver
        btcFCNI,    // fcni, dst, src ; conver                    
                                                           
        // FUNCTION / JUMPS         
        btcCALL,    // call, dst, func, [args.                   
        btcRET ,    // ret   src                                 
        btcJUMP,    // jump, lbl                                 
        btcJIFT,    // jift, lbl, src  ; jump                    
        btcJIFN,    // jifn, lbl, src  ; jump                    
                                                           
        // MATH
        btcCNUL,   // cnul, dst, src         

        // SIGNED MATH
        btcIEQU,   // iequ, dst, srca, srcb  
        btcIGTE,   // igte, dst, srca, srcb  
        btcILTE,   // ilte, dst, srca, srcb  
        btcIGET,   // iget, dst, srca, srcb  
        btcILET,   // ilet, dst, srca, srcb  
        btcIOR ,   // ior , dst, srca, srcb  
        btcIAND,   // iand, dst, srca, srcb  
        btcIADD,   // iadd, dst, srca, srcb  
        btcISUB,   // isub, dst, srca, srcb  
        btcIMUL,   // imul, dst, srca, srcb  
        btcIDIV,   // idiv, dst, srca, srcb  
        btcIMOD,   // imod, dst, srca, srcb  
        btcINOT,   // inot, dst, src         
        btcINEG,   // ineg, dst, src         
        btcISGN,   // isgn, dst, src         

        // UNSIGNED MATH
        btcUEQU,   // iequ, dst, srca, srcb  
        btcUGTE,   // igte, dst, srca, srcb  
        btcULTE,   // ilte, dst, srca, srcb  
        btcUGET,   // iget, dst, srca, srcb  
        btcULET,   // ilet, dst, srca, srcb  
        btcUOR ,   // ior , dst, srca, srcb  
        btcUAND,   // iand, dst, srca, srcb  
        btcUADD,   // iadd, dst, srca, srcb  
        btcUSUB,   // isub, dst, srca, srcb  
        btcUMUL,   // imul, dst, srca, srcb  
        btcUDIV,   // idiv, dst, srca, srcb  
        btcUMOD,   // imod, dst, srca, srcb  
        btcUNOT,   // inot, dst, src         
        btcUNEG,   // ineg, dst, src         
        btcUSGN,   // isgn, dst, src         
        
        // FLOATING POINT MATH
        btcFEQU,   // fequ, dst, srca, srcb  
        btcFGTE,   // fgte, dst, srca, srcb  
        btcFLTE,   // flte, dst, srca, srcb  
        btcFGET,   // fget, dst, srca, srcb  
        btcFLET,   // flet, dst, srca, srcb  
        btcFADD,   // fadd, dst, srca, srcb  
        btcFSUB,   // fsub, dst, srca, srcb  
        btcFMUL,   // fmul, dst, srca, srcb  
        btcFDIV,   // fdiv, dst, srca, srcb  
        btcFNOT,   // fnot, dst, src         
        btcFNEG,   // fneg, dst, src         
        btcFSGN,   // fsgn, dst, src         
    } mnemonic;
    union {
        data_t     data;
        usize      value;
        register_t dst;
        struct {
            register_t dst0;
            data_t literal;
        };
        struct {
            register_t dst1;
            usize offset;
        };
        struct {
            register_t dst3, srca, srcb;
            usize opsize;
        };
        struct {
            register_t dst4, src;
            usize size;
        };
        //TODO: call
    };
};

struct vm_t;
typedef struct function_t function_t;
struct function_t {
    usize hash; 
    pstring_t name;
    bool is_external;
    union {
        usize index; // bytecode index for the function
        bool (*external)(struct vm_t*, int nargs, value_t values[nargs]);
    };
};


typedef struct function_table_t function_table_t;
struct function_table_t {
    function_table_t *next;
    usize      count;
    function_t functions[255];
};

typedef struct data_table_t data_table_t;
struct data_table_t {
    data_table_t *next;
    u8 slots_in_use;
    data_t *slots[255];
};

typedef struct module_t module_t;
struct module_t {
    function_table_t functions;
    usize num_bytecodes;
    bytecode_t *bytecodes;
};

typedef struct stack_allocator_t stack_allocator_t;
struct stack_allocator_t {
    usize stack_pointer;
    usize end_of_storage;
    u8 *data;
};

typedef struct vm_t vm_t;
struct vm_t {
    stack_allocator_t stack;
    function_table_t functions;
    data_table_t data;
    usize num_bytecodes;
    bytecode_t *bytecodes;

    data_t *return_slot;
    bool nullflag;
    usize pc;
    usize sbp;
    // value_t dst, srca, srcb;
};

typedef struct label_t label_t;
struct label_t {
    usize index;
};

void *salloc(stack_allocator_t *, usize);

label_t btc_lbl_cur();
label_t btc_lbl_fun(function_t fn);

#define PSTD_TOGGLE(value, a, b) PSTD_DEFAULT_((PSTD_DEFAULT_, PSTD_ISEMPTY(value)))(a, b)
#define btc_lbl(out, ...) PSTD_TOGGLE(__VA_ARGS__, btc_lbl_fun, btc_lbl_cur)(out, ##__VA_ARGS__)

void btc_begin(void);
module_t btc_end(usize num_bytecodes, bytecode_t bytecodes[num_bytecodes]);

bytecode_t btc_noop(void);
bytecode_t btc_label(label_t *);
label_t    btf_label_from_function(function_t *);

#define btc_label(a)                    \
_Generic((a),                           \
label_t    *:   btc_label,              \
function_t *:   btf_label_from_function \
)

bytecode_t btc_push(usize);    // push, N        ; push N bytes onto the stack
bytecode_t btc_pop(usize);     // pop , N        ; pop  N bytes off  the stack

bytecode_t btc_data(register_t *, usize, usize);
bytecode_t btc_function(function_t *, pstring_t);
                                                           
// MOVE                                            
bytecode_t btc_nmov(register_t, register_t, usize);    // nmov, dst, src ; move i
bytecode_t btc_nufs(register_t, register_t, usize);    // nufs, dst, src ; move if null
bytecode_t btc_infs(register_t, usize);    // infs, dst, lit ; move literal unsigned int if null
bytecode_t btc_unfs(register_t, isize);    // unfs, dst, lit ; move literal signed int   if null
bytecode_t btc_fnfs(register_t, f64  );    // fnfs, dst, lit ; move literal float        if null
bytecode_t btc_umov(register_t, usize);    // umov, dst, lit ; move literal unsigned int 
bytecode_t btc_imov(register_t, isize);    // imov, dst, lit ; move literal signed int 
bytecode_t btc_fmov(register_t, f64  );    // fmov, dst, lit ; move literal float       
bytecode_t btc_vmov(register_t, register_t, usize);    // vmov, dst, src ; value 
bytecode_t btc_rmov(register_t, register_t, usize);    // rmov, dst, src ; move a
bytecode_t btc_dmov(register_t, register_t, usize);    // dmov, dst, src ; from v                   
                                                   
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
bytecode_t btc_cnul(register_t, register_t);   // cnul, dst, src         

// SIGNED MATH
bytecode_t btc_iequ(register_t, register_t, register_t, usize);   // iequ, dst, srca, srcb  
bytecode_t btc_igte(register_t, register_t, register_t, usize);   // igte, dst, srca, srcb  
bytecode_t btc_ilte(register_t, register_t, register_t, usize);   // ilte, dst, srca, srcb  
bytecode_t btc_iget(register_t, register_t, register_t, usize);   // iget, dst, srca, srcb  
bytecode_t btc_ilet(register_t, register_t, register_t, usize);   // ilet, dst, srca, srcb  
bytecode_t btc_ior (register_t, register_t, register_t, usize);   // ior , dst, srca, srcb  
bytecode_t btc_iand(register_t, register_t, register_t, usize);   // iand, dst, srca, srcb  
bytecode_t btc_iadd(register_t, register_t, register_t, usize);   // iadd, dst, srca, srcb  
bytecode_t btc_isub(register_t, register_t, register_t, usize);   // isub, dst, srca, srcb  
bytecode_t btc_imul(register_t, register_t, register_t, usize);   // imul, dst, srca, srcb  
bytecode_t btc_idiv(register_t, register_t, register_t, usize);   // idiv, dst, srca, srcb  
bytecode_t btc_imod(register_t, register_t, register_t, usize);   // imod, dst, srca, srcb  
bytecode_t btc_inot(register_t, register_t, usize);   // inot, dst, src         
bytecode_t btc_ineg(register_t, register_t, usize);   // ineg, dst, src         
bytecode_t btc_isgn(register_t, register_t, usize);   // isgn, dst, src         

// UNSIGNED MATH
bytecode_t btc_uequ(register_t, register_t, register_t, usize);   // iequ, dst, srca, srcb  
bytecode_t btc_ugte(register_t, register_t, register_t, usize);   // igte, dst, srca, srcb  
bytecode_t btc_ulte(register_t, register_t, register_t, usize);   // ilte, dst, srca, srcb  
bytecode_t btc_uget(register_t, register_t, register_t, usize);   // iget, dst, srca, srcb  
bytecode_t btc_ulet(register_t, register_t, register_t, usize);   // ilet, dst, srca, srcb  
bytecode_t btc_uor (register_t, register_t, register_t, usize);   // ior , dst, srca, srcb  
bytecode_t btc_uand(register_t, register_t, register_t, usize);   // iand, dst, srca, srcb  
bytecode_t btc_uadd(register_t, register_t, register_t, usize);   // iadd, dst, srca, srcb  
bytecode_t btc_usub(register_t, register_t, register_t, usize);   // isub, dst, srca, srcb  
bytecode_t btc_umul(register_t, register_t, register_t, usize);   // imul, dst, srca, srcb  
bytecode_t btc_udiv(register_t, register_t, register_t, usize);   // idiv, dst, srca, srcb  
bytecode_t btc_umod(register_t, register_t, register_t, usize);   // imod, dst, srca, srcb  
bytecode_t btc_unot(register_t, register_t, usize);   // inot, dst, src         
bytecode_t btc_uneg(register_t, register_t, usize);   // ineg, dst, src         
bytecode_t btc_usgn(register_t, register_t, usize);   // isgn, dst, src         

// FLOATING POINT MATH
bytecode_t btc_fequ(register_t, register_t, register_t);   // fequ, dst, srca, srcb  
bytecode_t btc_fgte(register_t, register_t, register_t);   // fgte, dst, srca, srcb  
bytecode_t btc_flte(register_t, register_t, register_t);   // flte, dst, srca, srcb  
bytecode_t btc_fget(register_t, register_t, register_t);   // fget, dst, srca, srcb  
bytecode_t btc_flet(register_t, register_t, register_t);   // flet, dst, srca, srcb  
bytecode_t btc_fadd(register_t, register_t, register_t);   // fadd, dst, srca, srcb  
bytecode_t btc_fsub(register_t, register_t, register_t);   // fsub, dst, srca, srcb  
bytecode_t btc_fmul(register_t, register_t, register_t);   // fmul, dst, srca, srcb  
bytecode_t btc_fdiv(register_t, register_t, register_t);   // fdiv, dst, srca, srcb  
bytecode_t btc_fnot(register_t, register_t);   // fnot, dst, src         
bytecode_t btc_fneg(register_t, register_t);   // fneg, dst, src         
bytecode_t btc_fsgn(register_t, register_t);   // fsgn, dst, src         


vm_t btc_init_vm(module_t *mod, usize stack_size);

bool vm_bind(vm_t *, pstring_t, bool (*)(vm_t*, int nargs, value_t values[nargs]));

function_t vm_find_function(vm_t *, pstring_t);
bool vm_run(vm_t *, function_t);

// vm_run(vm, (function_t){ .index = 0 });


