use std::env;
use std::error::Error;
use std::fs;
use std::io::Read;
use std::path::PathBuf;

pub type GenResult<A> = Result<A, Box<dyn Error>>;

pub fn get_input(day: i32) -> GenResult<PathBuf> {
    let mut parent = env::current_dir().unwrap();
    parent.push("inputs");

    if !fs::exists(&parent)? {
        fs::create_dir_all(&parent)?;
    }

    let input_path = parent.join(format!("{}.txt", day));

    if !fs::exists(&input_path)? {
        println!("{:?} does not exist. Will attempt to download it...", input_path);

        let session_path = parent.join("session.txt");
        println!("Reading {:?} to get AdventOfCode session...", session_path);
        let mut session_file = fs::File::open(&session_path)?;
        let mut session_string = String::new();
        session_file.read_to_string(&mut session_string)?;
        println!("Got session!");

        let mut input_file = fs::File::create(&input_path)?;
        println!("Downloading input to {:?}", input_path);

        let input_size = reqwest::blocking::Client
        ::new()
            .get(format!("https://adventofcode.com/2024/day/{}/input", day))
            .header("User-Agent", "@dylemma AdventOfCode 2024")
            .header("Cookie", format!("session={}", session_string))
            .send()?
            .copy_to(&mut input_file)?;
        println!("Downloaded {} bytes!", input_size);
    }

    Ok(input_path)
}