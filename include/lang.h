
/*

bool wrInitWindow(vm_t *, int nargs, object_t objects[nargs]) {
    value_t width  = objects[0].value;
    value_t height = objects[1].value;
    value_t str    = objects[2].value;
    InitWindow(width.ival, height.ival, str.sval.c_str);
    return true;
}

vm_bind(vm, "InitWindow", wrInitWindow);
vm_run_file(vm, file);

bytecode = btc_function(name);
register_t reg1 = btc_data(sizeof(usize), current_slot++);
register_t reg2 = btc_data(sizeof(usize), current_slot++);
register_t reg3 = btc_data(sizeof(usize), current_slot++);
btc_iadd(reg1, reg2, reg3);





// lang
extern fn InitWindow(width: int, height: int, title: string);

fn main {
    InitWindow(960, 54)
}

*/


