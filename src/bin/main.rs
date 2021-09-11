use anyhow::{ensure, Context, Result};

fn main() -> Result<()> {
    let mut arg: Vec<_> = std::env::args().collect();
    ensure!(arg.len() == 2, "Give me an argument!");
    let input = arg.pop().unwrap();
    let ast = xq::parser::parse_query(&input).with_context(|| "Parse query")?;
    println!("{:?}", ast);
    Ok(())
}
