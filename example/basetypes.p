
let i  : int    ;
let i  : uint   ;
let f  : float  ;
let ptr: *void  ;
let r  : ref int;
let str: string ;
// let arr: [5]int ;
// let drr: [*]int ; // dynamic array

struct thing {
    _a : int = 1; // private
     b : int = 5; // public
}


// define thing {
//     fn new({self._a, self.b});
//     property e {
//         set self._a = e;
//         get return self._a + self.b; 
//     }
// }

// 'unnamed' parameters
fn unnamed(arg0: int, arg1: int) ;


// named parameters
fn named({ arg0: int, arg1: int }) ;


// nullable parameters are optional when named
fn nullable_named({ arg0: int, arg1: ?int }) ;


// also if the last argument or if all the next parameters are null in 'unnamed' parameter list
// then it is also optional
fn nullable_unnamed(arg0: ?int, arg1: ?int) ;

// this however would cause an error
fn name(arg0: ?int, arg1: int, arg2: ?int) ;









fn fun({x: int, a: float, b: string, c: ?thing}): int {
    if c? return x + c!.b;
    else  return x;
}


// let inst : thing; // 'a' and 'b' are now respectively equal to 1 and 5 

fn main {
    unnamed(1, 2);
    named(arg0: 1, arg1: 2);
    nullable_named(arg0: 1); // this is fine arg1 will be null
    nullable_unnamed();      // also fine both arg0, and arg1 are null
    name();                  // error! arg1 has no value passed in
    name(0, 1);              // fine arg0 is explicity set to null, where arg2 is implicitly set to null
}


