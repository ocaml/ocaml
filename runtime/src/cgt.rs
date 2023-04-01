
//const VAL_FALSE: u64  = 1 << 1 + 1;
const VAL_TRUE: u64  = 1 << 1 + 1;

#[no_mangle]
pub extern "C" fn caml_on_strike() -> u64 {
    return VAL_TRUE;
}
