
struct thing {
    _a: int;
     b: float;
}

let global : thing;
let mut glob : thing;


let obj : int;

extern fn print(str: string);

fn with_ifs: int
    if global._a > 10 
         return 5;
    else return 4;

fn main: int {

    print("Hello, World!\n");

    extern fn dothing: ?int;
    let val : int = dothing()!;
    *&val = 6;

    return 0;
}





