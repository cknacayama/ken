use crate::RuntimeResult;
use crate::value::Value;

pub fn builtin_print(args: &[Value]) -> RuntimeResult<Value> {
    for arg in args {
        print!("{arg:?}");
    }
    Ok(Value::Unit)
}
