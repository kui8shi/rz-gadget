pub mod rzil;
use rzapi::api;
fn main() {
    println!("Hello, world!");
    let mut rzapi = api::RzApi::new(Some("/bin/ls")).unwrap();
    let mut rzil_ctx = rzil::RzILContext::new();
    rzil_ctx.bind_registers(&mut rzapi).unwrap();
    let inst = rzil_ctx.lift_inst(&mut rzapi, 0x4064).unwrap();
    dbg!(inst);
}
