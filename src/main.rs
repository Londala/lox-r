mod tokenizer;
mod interpreter;
mod parser;
mod stmt_types;
mod token_types;

use std::fs::File;
use clap::Parser;
use std::{env, path::PathBuf};
use std::io::Read;
use verbose_macros::{verbose, debug};

// fn get_relative_path(relpath: &str) -> PathBuf {
//     let mut path = env::current_exe().unwrap();
//     path.pop();
//     path.push(relpath);
//     path
// }

fn get_default_input_path() -> PathBuf {
    let mut path = env::current_exe().unwrap();
    println!("Initial executable path: {}", path.display());
    path.pop();
    path.pop();
    path.pop();
    path.push("lox");
    path.push("input.lox");
    path
}

#[derive(Parser, Debug)]
struct Cli {
    /// Path to the input file
    #[arg(short, long, default_value=get_default_input_path().into_os_string())]
    input_file: PathBuf,

    /// A flag to enable verbose mode
    #[arg(short, long, action = clap::ArgAction::SetTrue)]
    verbose: bool,

    /// A flag to enable debug mode (also enables verbose by default)
    #[arg(short, long, action = clap::ArgAction::SetTrue)]
    debug: bool,

}

fn read_file(input_file: PathBuf) -> String {
    let display = input_file.display();
    verbose!("Searching for input file: {}", display);
    if input_file.exists() {
        verbose!("Input file exists!");
    } else {
        panic!("Input file does NOT exist. {}", display);
    }

    // Open the path in read-only mode, returns `io::Result<File>`
    let mut file = match File::open(&input_file) {
        Err(why) => panic!("couldn't open {}: {}", display, why),
        Ok(file) => file,
    };
    verbose!("Reading input file: {}", display);

    // Read the file contents into a string, returns `io::Result<usize>`
    let mut s = String::new();
    match file.read_to_string(&mut s) {
        Err(why) => panic!("couldn't read {}: {}", display, why),
        Ok(_) => verbose!("{} contains:\n{}\n", display, s),
    }
    return s;
}
fn run_lox(input_file: PathBuf) {
    let code = read_file(input_file);
    verbose!("Tokenizing...");
    let tokens = tokenizer::tokenize(code);
    verbose!("Tokens: \n{:?}", tokens);
    let statements = parser::parse(tokens);
    verbose!("Statements: \n{:?}", statements);

}

fn main() {
    let args = Cli::parse();

    println!("Input file: {:?}", args.input_file);
    // println!("Output directory: {:?}", args.output_dir);

    verbose_macros::set_verbose(args.debug || args.verbose);
    verbose_macros::set_debug(args.debug);

    println!("Verbose mode: {}", (args.debug || args.verbose));
    verbose!("This is a verbose message.");
    println!("Debug mode: {}", args.debug);
    debug!("This is a debug message.");

    run_lox(args.input_file);
}