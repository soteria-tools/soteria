fn takes_dyn_fun(fun: Box<dyn FnOnce() -> u32>) -> u32 {
    fun()
}

pub fn unit_to_u32() -> u32 {
    5
}

fn main() {
    assert!(takes_dyn_fun(Box::new(&unit_to_u32)) == 5)
}
