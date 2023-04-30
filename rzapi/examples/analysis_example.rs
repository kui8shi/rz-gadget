//! Example to showcase use of different, fine-grained analysis in radare.

extern crate rzapi;
extern crate rzpipe;
extern crate serde_json;

use rzapi::api_trait::RzApi;
use rzpipe::rz::Rz;

fn main() {
    {
        let path = "/bin/ls";
        let mut rz = Rz::new(Some(path)).expect("Failed to spawn rz");
        rz.analyze_all().unwrap();
        println!("{:#?}", rz.fn_list());
    }
    {
        let path = "/bin/ls";
        let mut rz = Rz::new(Some(path)).expect("Failed to spawn rz");
        rz.analyze_and_autoname().unwrap();
        println!("{:#?}", rz.fn_list());
    }
    {
        let path = "/bin/ls";
        let mut rz = Rz::new(Some(path)).expect("Failed to spawn rz");
        rz.analyze_function_calls().unwrap();
        println!("{:#?}", rz.fn_list());
    }
    {
        let path = "/bin/ls";
        let mut rz = Rz::new(Some(path)).expect("Failed to spawn rz");
        rz.analyze_data_references().unwrap();
        println!("{:#?}", rz.fn_list());
    }
    {
        let path = "/bin/ls";
        let mut rz = Rz::new(Some(path)).expect("Failed to spawn rz");
        rz.analyze_references_esil().unwrap();
        println!("{:#?}", rz.fn_list());
    }
    {
        let path = "/bin/ls";
        let mut rz = Rz::new(Some(path)).expect("Failed to spawn rz");
        rz.analyze_function_preludes().unwrap();
        println!("{:#?}", rz.fn_list());
    }
    {
        let path = "/bin/ls";
        let mut rz = Rz::new(Some(path)).expect("Failed to spawn rz");
        rz.analyze_function_references().unwrap();
        println!("{:#?}", rz.fn_list());
    }
    {
        let path = "/bin/ls";
        let mut rz = Rz::new(Some(path)).expect("Failed to spawn rz");
        rz.analyze_symbols().unwrap();
        println!("{:#?}", rz.fn_list());
    }
    {
        let path = "/bin/ls";
        let mut rz = Rz::new(Some(path)).expect("Failed to spawn rz");
        rz.analyze_consecutive_functions().unwrap();
        println!("{:#?}", rz.fn_list());
    }
}
