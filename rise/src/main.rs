mod context;
mod engine;
mod error;
mod explorer;
mod map;
mod paged_map;
mod registers;
mod rzil;
mod to_z3;
mod variables;

use error::Result;
fn main() -> Result<()> {
    /*
    println!("Hello, world!");
    let mut rzapi = api::RzApi::new(Some("/bin/ls"), None).unwrap();
    let mut rzil_ctx = rzil::RzILContext::new();
    rzil_ctx.bind_registers(&mut rzapi)?;
    let inst = rzil_ctx.lift_n_insts(&mut rzapi, 0x67d4, 1)?;
    dbg!(inst);
    */
    Ok(())
}
