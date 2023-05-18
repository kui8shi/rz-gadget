extern crate rzapi;
extern crate rzpipe;
extern crate serde_json;

use rzapi::api::RzApi;
fn main() {
    let path = "/bin/ls";
    let mut rz = RzApi::new(Some(path)).expect("Failed to spawn rz");
    rz.analyze_all();
    println!("{:#?}", rz.get_registers());
    println!("{:#?}", rz.get_info());
    println!("{:#?}", rz.get_flags());
    println!("{:#?}", rz.get_functions());
    println!("{:#?}", rz.get_symbols());
    println!("{:#?}", rz.get_imports());
    println!("{:#?}", rz.get_exports());
    println!("{:#?}", rz.get_relocs());
    println!("{:#?}", rz.get_entrypoint());
    println!("{:#?}", rz.get_libraries());
}
