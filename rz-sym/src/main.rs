/*
pub mod regstore {
    pub mod regstore;
    pub mod regfile;
}
pub mod explorer {
    //pub mod explorer;
}
pub mod context;
*/
pub mod memory;
pub mod rzil;
use anyhow::Result;
use rzapi::api;
fn main() -> Result<()> {
    println!("Hello, world!");
    let mut rzapi = api::RzApi::new(Some("/bin/ls"), None).unwrap();
    let mut rzil_ctx = rzil::RzILContext::new();
    rzil_ctx.bind_registers(&mut rzapi)?;
    let inst = rzil_ctx.lift_n_insts(&mut rzapi, 0x67d4, 1)?;
    dbg!(inst);
    Ok(())
}
