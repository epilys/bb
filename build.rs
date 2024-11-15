const LINE_BREAK_TABLE_URL: &str = "http://www.unicode.org/Public/UCD/latest/ucd/LineBreak.txt";
use std::{
    fs::File,
    io::{prelude::*, BufReader},
    path::PathBuf,
    process::Command,
};

include!("src/text_processing/types.rs");

fn main() -> Result<(), std::io::Error> {
    let mod_path = PathBuf::from("src/text_processing/tables.rs");
    if mod_path.exists() {
        eprintln!(
            "{} already exists, delete it if you want to replace it.",
            mod_path.display()
        );
        std::process::exit(0);
    }
    let mut tmpdir_path = PathBuf::from(
        std::str::from_utf8(&Command::new("mktemp").arg("-d").output()?.stdout)
            .unwrap()
            .trim(),
    );
    tmpdir_path.push("LineBreak.txt");
    Command::new("curl")
        .args(["-o", tmpdir_path.to_str().unwrap(), LINE_BREAK_TABLE_URL])
        .output()?;

    let file = File::open(&tmpdir_path)?;
    let buf_reader = BufReader::new(file);

    let mut line_break_table: Vec<(u32, u32, LineBreakClass)> = Vec::with_capacity(3800);
    for line in buf_reader.lines() {
        let line = line.unwrap();
        if line.starts_with('#') || line.starts_with(' ') || line.is_empty() {
            continue;
        }
        let tokens: &str = line.split_whitespace().next().unwrap();

        let semicolon_idx: usize = tokens.chars().position(|c| c == ';').unwrap();
        // LineBreak.txt list is ascii encoded so we can assume each char takes one
        // byte:
        let chars_str: &str = &tokens[..semicolon_idx];

        let mut codepoint_iter = chars_str.split("..");

        let first_codepoint: u32 =
            u32::from_str_radix(std::dbg!(codepoint_iter.next().unwrap()), 16).unwrap();

        let sec_codepoint: u32 = codepoint_iter
            .next()
            .map(|v| u32::from_str_radix(std::dbg!(v), 16).unwrap())
            .unwrap_or(first_codepoint);
        let class = &tokens[semicolon_idx + 1..semicolon_idx + 1 + 2];
        line_break_table.push((first_codepoint, sec_codepoint, LineBreakClass::from(class)));
    }

    let mut file = File::create(&mod_path)?;
    file.write_all(b"use crate::types::LineBreakClass::*;\n")
        .unwrap();
    file.write_all(b"use crate::types::LineBreakClass;\n\n")
        .unwrap();
    file.write_all(b"const line_break_rules: &'static [(u32, u32, LineBreakClass)] = &[\n")
        .unwrap();
    for l in &line_break_table {
        file.write_all(format!("    (0x{:X}, 0x{:X}, {:?}),\n", l.0, l.1, l.2).as_bytes())
            .unwrap();
    }
    file.write_all(b"];").unwrap();
    std::fs::remove_file(&tmpdir_path).unwrap();
    tmpdir_path.pop();
    std::fs::remove_dir(&tmpdir_path).unwrap();
    Ok(())
}
