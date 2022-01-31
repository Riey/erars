fn main() {
    #[cfg(unix)]
    {
        println!("cargo:rustc-link-lib=vulkan");
        println!("cargo:rustc-link-lib=EGL");
        println!("cargo:rustc-link-lib=wayland-client");
        println!("cargo:rustc-link-lib=xkbcommon");
        println!("cargo:rustc-link-lib=X11");
        println!("cargo:rustc-link-lib=Xcursor");
        println!("cargo:rustc-link-lib=Xrandr");
        println!("cargo:rustc-link-lib=Xi");
        println!("cargo:rustc-link-lib=Xxf86vm");
    }
}
