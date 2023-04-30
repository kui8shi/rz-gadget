extern crate rzapi;
extern crate rzpipe;
extern crate serde_json;

use rzapi::api_trait::RzApi;
use rzpipe::rz::Rz;

fn main() {
    let path = "/bin/ls";
    let mut rz = Rz::new(Some(path)).expect("Failed to spawn rz");
    rz.init();
    rz.analyze();
    println!("{:#?}", rz.reg_info());
    println!("{:#?}", rz.bin_info());
    println!("{:#?}", rz.flag_info());
    println!("{:#?}", rz.fn_list());
    println!("{:#?}", rz.symbols());
    println!("{:#?}", rz.imports());
    println!("{:#?}", rz.exports());
    println!("{:#?}", rz.relocs());
    println!("{:#?}", rz.entrypoint());
    println!("{:#?}", rz.libraries());
}
