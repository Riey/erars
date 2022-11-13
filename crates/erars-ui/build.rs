fn main() {
    println!("cargo:rerun-if-changed=ui.fbs");
    std::process::Command::new("flatc")
        .arg("--rust")
        .arg("-o")
        .arg(std::env::var("OUT_DIR").unwrap())
        .arg("ui.fbs")
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
}
