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
fn main() -> Result<()> {
    let mut rise = Rise::<state::StateZ3Backend>::new(Some("/bin/ls"))?;
    rise.set_register("rsp", 0x00007fffffffdbd0);
    rise.run(engine::Mode::Block)?;
    Ok(())
}
