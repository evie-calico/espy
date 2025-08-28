use clap::{Args, Parser};
use std::{fs, path::Path};

#[derive(Parser)]
#[clap(version, about)]
struct Cli {
    #[clap(flatten)]
    input: Input,
}

#[derive(Args)]
#[group(required = true, multiple = false)]
struct Input {
    #[clap(short)]
    command: Option<Box<str>>,

    program: Option<Box<Path>>,
}

fn main() {
    let cli = Cli::parse();
    let source = if let Some(program) = cli.input.program {
        fs::read_to_string(program).unwrap().into_boxed_str()
    } else {
        cli.input
            .command
            .expect("either field of input must be Some")
    };
    let result = espy::Program::try_from(&*source)
        .unwrap()
        .eval()
        .unwrap();
    println!("{result:?}");
}
