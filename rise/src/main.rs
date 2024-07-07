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
    rise.run(engine::Mode::Block)?;
    Ok(())
}
