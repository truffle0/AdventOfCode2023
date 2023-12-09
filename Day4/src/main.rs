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
	num: i32,
	winning: Vec<i32>,
	present: Vec<i32>,
	matches: usize,
}

impl Card {
	fn from_string(line: &str) -> Result<Card> {
		// Separate 'Card #:' from the start of the string
		let (_, numbers) = match line.split_once(':') {
			None => return Err(Error::new(UnexpectedEof, "Unexpected end-of-line!")),
			Some((prefix, suffix)) => (prefix, suffix),
		};

		// Separate '# # #... | # # # ...'
		let (prefix, suffix) = match numbers.split_once('|') {
			None => return Err(Error::new(InvalidData, "Invalid format! Missing '|'")),
			Some((first, last)) => (first, last),
		};

		// Allocate vectors for the sets of numbers
		let winning = parse_numbers(prefix)?;
		let present = parse_numbers(suffix)?;
		
		// Precompute the number of matches (save alot of time later)
		let mut matches = 0;
		for x in present {
			if winning.contains(&x) {
				matches += 1;
			}
		}
		
		// Parse numbers and create struct
		// winning numbers from the start and present numbers from the end of the string
		Ok(Card {
			num: -1,
			winning: parse_numbers(prefix)?,
			present: parse_numbers(suffix)?,
			matches: matches,
		})
	}

	fn score(&self) -> u32 {
		// This function was written before I pre-computed the matches
		let mut total = 0;
		for i in self.present.iter() {
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
	let mut i = 1;	// Counter for the card number

	for line in source {
		let mut next = Card::from_string(&line?)?;
		next.num = i; i += 1;

		cards.push(next);
	}

	return Ok(cards)
}

// PART 1
fn calculate_total_score(cards: &Vec<Card>) -> u32 {
	let mut total = 0;
	for card in cards {
		total += card.score();
	}

	return total;
}

// PART 2
fn calculate_card_copies(cards: &Vec<Card>) -> Result<u64> {
	/* Implements the following rule for Part 2
	There's no such thing as "points". Instead, scratchcards only cause you to win more scratchcards equal to the number of winning numbers you have.

	Specifically, you win copies of the scratchcards below the winning card equal to the number of matches.
	So, if card 10 were to have 5 matching numbers, you would win one copy each of cards 11, 12, 13, 14, and 15.

	Copies of scratchcards are scored like normal scratchcards and have the same card number as the card they copied.
	So, if you win a copy of card 10 and it has 5 matching numbers, it would then win a copy of the same cards that the
	original card 10 won: cards 11, 12, 13, 14, and 15. This process repeats until none of the copies cause you to win any more cards.
	(Cards will never make you copy a card past the end of the table.)
	*/

	// Use a stack structure to store the indexes of copies
	let mut copies = Vec::with_capacity(cards.len());
	for _ in 0..cards.len() { copies.push(1); }		// Initalize vector with 1s
	
	// Outer loop to iterate once over all the cards
	for i in 0..cards.len() {
		let matches = cards[i].matches;

		if i + matches > cards.len() {
			return Err(Error::new(InvalidData, "References cards that don't exist!"));
		}

		// Add 1 copy to every x following index for each copy in the current
		for x in 0..matches {
			copies[i + x + 1] += copies[i];
		}
	}

	let mut total = 0;
	for x in copies {
		total += x;
	}

	Ok(total)
}

fn main() {
	let args:Vec<String> = env::args().collect();
	let path = if args.len() > 1 {
		Some(args[1].as_str())
	} else {
		None
	};

	let cards = parse_cards(open_reader(path).unwrap()).unwrap();

	println!("Part 1: {}", calculate_total_score(&cards));

	println!("Part 2: {}", calculate_card_copies(&cards).unwrap());
}