pub fn get_about_text() -> &'static str {
    include_str!(concat!(env!("OUT_DIR"), "/about.txt"))
}

