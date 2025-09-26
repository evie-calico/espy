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

#[derive(Default, Debug)]
struct EspyshLibContainer {
    std: espystandard::Lib,
}

impl espy::Extern for EspyshLibContainer {
    fn index<'host>(
        &'host self,
        index: espy::Value<'host>,
    ) -> Result<espy::Value<'host>, espy::interpreter::Error<'host>> {
        let index = index.into_str()?;
        match &*index {
            "std" => Ok(espy::Value::borrow(&self.std)),
            _ => Err(espy::Error::IndexNotFound {
                index: index.into(),
                container: espy::Value::Borrow(self),
            }),
        }
    }

    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::write!(f, "espysh libraries")
    }
}

fn main() {
    static LIB: EspyshLibContainer = EspyshLibContainer {
        std: espystandard::Lib,
    };
    let cli = Cli::parse();
    let source = if let Some(program) = cli.input.program {
        fs::read_to_string(program).unwrap().into_boxed_str()
    } else {
        cli.input
            .command
            .expect("either field of input must be Some")
    };
    let result = espy::Program::try_from(&*source).unwrap().eval().unwrap();
    if let espy::Value::Function(function) = result {
        let result = std::rc::Rc::unwrap_or_clone(function)
            .piped(espy::Value::borrow(&LIB))
            .piped(espy::Value::<'static>::Unit)
            .eval()
            .unwrap();
        println!("{result:#?}");
    } else {
        println!("{result:#?}");
    }
}
