use std::env;
use std::fs::File;
use std::io::Error;
use std::io::*;
use std::io::ErrorKind::{InvalidData, UnexpectedEof};

// General function to parse whitespace-separated numbers
fn parse_numbers(line: &str) -> Result<Vec<i32>> {
	let mut numbers = Vec::new();
	let mut num = String::new();

	for c in line.chars() {
		if c.is_numeric() {
			num.push(c);
		} else if c.is_whitespace() {
			if num.len() > 0 {
				numbers.push(match num.parse::<i32>() {
					Err(why) => return Err(Error::new(InvalidData, why.to_string())),
					Ok(num) => num,
				});
				num.clear();
			}
		} else {
			return Err(Error::new(InvalidData, format!("Invalid format! Unexpected character '{}'", c)));
		}
	}

	if num.len() > 0 {
		numbers.push(match num.parse::<i32>() {
			Err(why) => return Err(Error::new(InvalidData, why.to_string())),
			Ok(num) => num,
		});
	}

	Ok(numbers)
}

struct Card {
	winning: Vec<i32>,
	present: Vec<i32>,
}

impl Card {
	fn from_string(line: &str) -> Result<Card> {
		// Separate 'Card #:' from the start of the string
		let line = match line.split_once(':') {
			None => return Err(Error::new(UnexpectedEof, "Unexpected end-of-line!")),
			Some((_, suffix)) => suffix,
		};
		let (prefix, suffix) = match line.split_once('|') {
			None => return Err(Error::new(InvalidData, "Invalid format! Missing '|'")),
			Some((first, last)) => (first, last),
		};

		// Parse numbers and create struct
		// winning numbers from the start and present numbers from the end of the string
		Ok(Card {
			winning: parse_numbers(prefix)?,
			present: parse_numbers(suffix)?,
		})
	}

	fn calculate_score(self) -> i32 {
		let mut total = 0;
		for i in self.present {
			if self.winning.contains(&i) {
				if total == 0 {
					total += 1
				} else {
					total *= 2
				}
			}
		}

		return total;
	}
}

fn open_reader(path: Option<&str>) -> Result<impl Iterator<Item = Result<String>>> {
	let source: Box<dyn Read> = match path {
		None => Box::new(stdin()),
		Some(p) => Box::new(File::open(p)?),
	};

	Ok(BufReader::new(source).lines())
}

fn parse_cards(source: impl Iterator<Item = Result<String>>) -> Result<Vec<Card>> {
	let mut cards = Vec::new();

	for line in source {
		cards.push(Card::from_string(&line?)?);
	}

	return Ok(cards)
}

fn calculate_total_score(cards: Vec<Card>) -> i32 {
	let mut total = 0;
	for card in cards {
		total += card.calculate_score();
	}

	return total;
}

fn main() {
	let args:Vec<String> = env::args().collect();
	let path = if args.len() > 1 {
		Some(args[1].as_str())
	} else {
		None
	};

	let cards = parse_cards(open_reader(path).unwrap()).unwrap();

	println!("Part 1: {}", calculate_total_score(cards));
}