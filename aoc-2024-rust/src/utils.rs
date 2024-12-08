pub fn load_file(file_path: &str) -> String {
    std::fs::read_to_string(file_path).unwrap()
}