pub mod rzil;
use rzapi::api;
fn main() {
    println!("Hello, world!");
    let mut rzapi = api::RzApi::new(Some("/bin/ls"), None).unwrap();
    let mut rzil_ctx = rzil::RzILContext::new();
    rzil_ctx.bind_registers(&mut rzapi).unwrap();
    let inst = rzil_ctx
        .lift_n_insts(&mut rzapi, 0x4e44, 9)
        .map_err(|e| panic!("{}", e.to_string()))
        .unwrap();
    dbg!(inst);
}
