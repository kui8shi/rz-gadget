mod convert;
mod engine;
mod error;
mod explorer;
mod map;
mod registers;
mod rzil;
mod state;
mod variables;

use engine::Rise;
use error::Result;
use state::solver::Z3;
fn main() -> Result<()> {
    let mut rise = Rise::<state::StateZ3Backend<Z3>>::new(Some("/bin/ls"))?;
    //rise.set_register("rsp", 0x11000);
    rise.run(engine::Mode::Block)?;
    Ok(())
}
