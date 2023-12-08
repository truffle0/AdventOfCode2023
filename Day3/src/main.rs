use std::env;
use std::fs::File;
use std::io::Error;
use std::io::*;

/*
    This is my first proper program in rust.
    It took me *quite* a long time to get this right, and was a massive learning curve.

    The number of times I had to rewrite this program is somewhat embarassing (hence the delay ;-;)

	Final result has alot of redundant stuff, and is generally very inefficient.
	I tried *many* ways of doing this, but ended on a pretty inefficient result after alot of roadblocks.
*/

enum Item {
    Symbol(char),
    Digit(char),
    Space,
}

impl Item {
    fn is_symbol(&self) -> bool {
        match self {
            Item::Symbol(_) => true,
            _ => false,
        }
    }

	fn is_digit(&self) -> bool {
		match self {
			Item::Digit(_) => true,
			_ => false,
		}
	}

	fn char(&self) -> char {
		match self {
			Item::Symbol(c) => *c,
			Item::Digit(c) => *c,
			Item::Space => '.',
		}
	}
}

/*
Had alot of trouble with index out of bounds problems,
so I just decided to implement this as a struct and implement a bunch of checking
and safe operations functions
*/
struct Plan {
    width: usize,
    height: usize,
    grid: Vec<Vec<Item>>,
}

impl Plan {
    fn new(width: usize, height: usize) -> Plan {
        let mut plan = Vec::with_capacity(height);

        for i in 0..height {
            // Initialize the row
            plan.push(Vec::with_capacity(width));

            // Fill the row with Spaces by default
            for _ in 0..width {
                plan[i].push(Item::Space);
            }
        }

        Plan {
            height: height,
            width: width,
            grid: plan,
        }
    }

    fn get(&self, row: usize, col: usize) -> Result<&Item> {
        self.check_bounds(row, col)?;
		self.validate_width(row)?;

        Ok(&self.grid[row][col])
    }

    fn set(&mut self, value: Item, row: usize, col: usize) -> Result<()> {
        self.check_bounds(row, col)?;

        self.grid[row][col] = value;
        Ok(())
    }

    fn check_adjacent_symbols(&self, row: usize, col: usize) -> Result<bool> {
        self.check_bounds(row, col)?;

        // Check to the left and right in the same row
        if col > 0 && self.grid[row][col - 1].is_symbol() {
            return Ok(true);
        }
        if col < self.width - 1 && self.grid[row][col + 1].is_symbol() {
            return Ok(true);
        }

        // Check below indexes, including diagonals (if the row exists)
        if row > 0 {
            if col > 0 && self.grid[row - 1][col - 1].is_symbol() {
                return Ok(true);
            }
            if self.grid[row - 1][col].is_symbol() {
                return Ok(true);
            }
            if col < self.width - 1 && self.grid[row - 1][col + 1].is_symbol() {
                return Ok(true);
            }
        }

        // Check above indexes, including diagonals (if the row exists)
        if row < self.height - 1 {
            if col > 0 && self.grid[row + 1][col - 1].is_symbol() {
                return Ok(true);
            }
            if self.grid[row + 1][col].is_symbol() {
                return Ok(true);
            }
            if col < self.width - 1 && self.grid[row + 1][col + 1].is_symbol() {
                return Ok(true);
            }
        }

        Ok(false)
    }

	fn find_adjacent_numbers(&self, row:usize, col:usize) -> Result<Vec<i32>> {
		self.check_bounds(row, col)?;
		let mut digits = Vec::new();

		// Determine the coords of digits surrounding the position
		if col > 0 && self.grid[row][col-1].is_digit() {
			digits.push((row, col-1));
		}
		if self.grid[row][col].is_digit() {
			digits.push((row, col));
		}
		if col < self.width - 1 && self.grid[row][col+1].is_digit() {
			digits.push((row, col+1));
		}

		if row > 0 {
			if col > 0 && self.grid[row-1][col-1].is_digit() {
				digits.push((row-1, col-1));
			}
			if self.grid[row-1][col].is_digit() {
				digits.push((row-1, col));
			}
			if col < self.width - 1 && self.grid[row-1][col+1].is_digit() {
				digits.push((row-1, col+1));
			}
		}

		if row < self.height - 1 {
			if col > 0 && self.grid[row+1][col-1].is_digit() {
				digits.push((row+1, col-1));
			}
			if self.grid[row+1][col].is_digit() {
				digits.push((row+1, col));
			}
			if col < self.width - 1 && self.grid[row+1][col+1].is_digit() {
				digits.push((row+1, col+1));
			}
		}

		// Once digits have been found, construct numbers and eliminate duplicates
		let mut nums = Vec::new();
		let mut starts = Vec::new();
		for (row, col) in digits {
			let (i, start) = self.walk_read_number(row, col)?;

			if !starts.contains(&(row, start)) {
				nums.push(i);
				starts.push((row, start));
			}
		}

		Ok(nums)
	}

	fn walk_read_number(&self, row:usize, col:usize) -> Result<(i32, usize)> {
		self.check_bounds(row, col)?;

		// Find the start of the number
		let mut start = col;
		while start > 0 {
			if self.grid[row][start - 1].is_digit() {
				start -= 1;
			} else {
				break;
			}
		}

		let mut num = String::new();
		for i in start..self.grid[row].len() {
			if self.grid[row][i].is_digit() {
				num.push(self.grid[row][i].char());
			} else {
				break;
			}
		}

		if num.len() == 0 {
			return Err(Error::new(ErrorKind::InvalidData, "Value provided is not a number!"));
		}

		match num.parse::<i32>() {
			Err(why) => Err(Error::new(ErrorKind::InvalidData, why.to_string())),
			Ok(i) => Ok((i, start)),
		}
	}

    fn check_bounds(&self, row: usize, col: usize) -> Result<()> {
        if row < self.width && col < self.height {
			self.validate_width(row)?;
            Ok(())
        } else {
            Err(Error::new(
                ErrorKind::InvalidData,
                format!(
                    "Index {},{} out of bounds! Max: {}x{}",
                    row, col, self.height, self.width
                ),
            ))
        }
    }

    fn validate_width(&self, row: usize) -> Result<()> {
        // Basic bounds check
        if row <= self.height && self.grid[row].len() == self.width {
            Ok(())
        } else {
            Err(Error::new(
                ErrorKind::InvalidData,
                format!(
                    "Row {} has incorrect width {}, should be {}.",
                    row,
                    self.grid[row].len(),
                    self.width
                ),
            ))
        }
    }
}

fn open_reader(path: Option<&String>) -> Result<Vec<String>> {
    // Creates both a buffered reader object and returns the number of lines in the file by pre-reading.
    let mut buffer: Vec<String> = Vec::new();

    let source: Box<dyn Read> = match path {
        None => Box::new(stdin()),
        Some(f) => Box::new(File::open(f)?),
    };

    for line in BufReader::new(source).lines() {
        buffer.push(line?);
    }

    Ok(buffer)
}

fn read_the_plan(source: Vec<String>) -> Result<Plan> {
    // I had to make this intermediate function to read the input into a list.
    // Rust proved to be way too strict on String to allow me to read the plan in a simpler way.

    // Estalish height and width values, then allocate a plan struct
    let height = source.len();
    let width = source[0].len();
	let mut plan = Plan::new(width, height);

	for row in 0..source.len() {
		if source[row].len() != width {
			return Err(Error::new(
				ErrorKind::InvalidData,
				"Inconsistent line lengths!"
			));
		}

		let mut col = 0;
		for c in source[row].chars() {
			if c.is_numeric() {
				plan.set(Item::Digit(c), row, col)?;
			} else if c != '.' {
				plan.set(Item::Symbol(c), row, col)?;
			}
			// Skip setting to Item::Space, as this is the default
			col += 1;
		}
	}

    return Ok(plan);
}

fn add_valid_numbers(plan: &Plan) -> Result<i32> {
    let mut total = 0;

    let mut adjacent = false;
    let mut num = String::new();

	for row in 0..plan.height {

		for col in 0..plan.width {
			match plan.get(row, col)? {
				Item::Digit(c) => {
					num.push(*c);
					if plan.check_adjacent_symbols(row, col)? {
						adjacent = true;
					}
				},
				_ => {
					if adjacent {
						total += match num.parse::<i32>() {
							Err(why) => return Err(Error::new(ErrorKind::InvalidData, why.to_string())),
							Ok(i) => i,
						};
						adjacent = false;
					}
					num.clear();
				}
			}
		}

		if adjacent {
			total += match num.parse::<i32>() {
				Err(why) => return Err(Error::new(ErrorKind::InvalidData, why.to_string())),
				Ok(i) => i,
			};
			adjacent = false;
		}

		num.clear();
	}

    Ok(total)
}

fn sum_gear_ratios(plan: &Plan) -> Result<i64> {
	
	let mut total = 0;
	for row in 0..plan.height {

		for col in 0..plan.width {
			match plan.get(row,col)? {
				&Item::Symbol(c) => {
					if c == '*' {
						let nums = plan.find_adjacent_numbers(row, col)?;

						if nums.len() == 2 {
							total += (nums[0] * nums[1]) as i64;
						} else {
						}
					}
				},
				_ => {},
			}
		}
	}

	Ok(total)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = if args.len() > 1 && args[1] != "-" {
        Some(&args[1])
    } else {
        None
    };

	// Part 1
    let source = open_reader(path).unwrap();
    let plan = read_the_plan(source).unwrap();

    println!("Part 1: {}", add_valid_numbers(&plan).unwrap());

	// Part 2
	println!("Part 2: {}", sum_gear_ratios(&plan).unwrap());
}
