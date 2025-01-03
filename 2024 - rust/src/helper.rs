use log::{debug, info};
use std::env;
use std::error::Error;
use std::fs;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

pub type GenError = Box<dyn Error>;
pub type GenResult<A> = Result<A, GenError>;

pub fn get_input(day: u32) -> GenResult<PathBuf> {
    let mut parent = env::current_dir().unwrap();
    parent.push("inputs");

    if !fs::exists(&parent)? {
        fs::create_dir_all(&parent)?;
    }

    let input_path = parent.join(format!("{}.txt", day));

    if !fs::exists(&input_path)? {
        info!(
            "{:?} does not exist. Will attempt to download it...",
            input_path
        );

        let session_path = parent.join("session.txt");
        debug!("Reading {:?} to get AdventOfCode session...", session_path);
        let mut session_file = File::open(&session_path)?;
        let mut session_string = String::new();
        session_file.read_to_string(&mut session_string)?;
        debug!("Got session!");

        let mut input_file = File::create(&input_path)?;
        debug!("Downloading input to {:?}", input_path);

        let input_size = reqwest::blocking::Client::new()
            .get(format!("https://adventofcode.com/2024/day/{}/input", day))
            .header("User-Agent", "@dylemma AdventOfCode 2024")
            .header("Cookie", format!("session={}", session_string))
            .send()?
            .copy_to(&mut input_file)?;
        debug!("Downloaded {} bytes!", input_size);
        info!("Download complete!");
    }

    Ok(input_path)
}
