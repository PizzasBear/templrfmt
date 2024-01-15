use std::{
    fs::File,
    io::{self, Read, Seek, SeekFrom, Write},
};

use anyhow::Result;
use clap::Parser;
use glob::glob;
use rustfmt_wrapper::rustfmt;
use templr_formatter::unparse_templ_macros;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// A space separated list of file, directory or glob
    #[arg(required_unless_present = "stdin")]
    input_patterns: Option<Vec<String>>,

    /// Format stdin and write to stdout
    #[arg(short, long, default_value = "false")]
    stdin: bool,

    /// Format with rustfmt
    #[arg(short, long, default_value = "false", requires = "stdin")]
    rustfmt: bool,

    /// Doesn't print any dignostics.
    #[arg(
        short,
        long,
        default_value = "false",
        default_value_if("stdin", "true", "true")
    )]
    quiet: bool,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    for pat in cli.input_patterns.iter().flatten() {
        for entry in glob(pat)? {
            let entry = entry?;
            let mut file = File::options().read(true).write(true).open(&entry)?;
            let mut source = String::new();
            file.read_to_string(&mut source)?;
            if cli.rustfmt {
                source = rustfmt(source)?;
            }
            source = unparse_templ_macros(&source)?;
            if cli.rustfmt {
                source = rustfmt(source)?;
            }
            file.set_len(0)?;
            file.seek(SeekFrom::Start(0))?;
            file.write_all(source.as_bytes())?;
            if !cli.quiet {
                println!("Formatted: `{}`", entry.display());
            }
        }
    }
    if cli.stdin {
        let mut source = String::new();
        io::stdin().read_to_string(&mut source)?;
        if cli.rustfmt {
            source = rustfmt(source)?;
        }
        source = unparse_templ_macros(&source)?;
        if cli.rustfmt {
            source = rustfmt(source)?;
        }
        io::stdout().write_all(source.as_bytes())?;
    }

    Ok(())
}
