use espy_eyes::Lexer;

fn main() {
    for i in std::env::args() {
        for token in Lexer::from(i.as_str()) {
            print!("{token:?}, ");
        }
        println!();
    }
}
