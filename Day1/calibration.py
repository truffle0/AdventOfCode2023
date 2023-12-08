import sys
import re
from typing import Generator

### Part 1 ###
def part_one(source) -> int:
	# - Reads each line of input
	# - Adds the first and last numbers
	# - returns the total sum

	total = 0
	for line in source:
		line = line.strip()

		# Empty line
		if not line: break

		# Find the index of the first digit
		first = None
		for c in line:
			if c.isnumeric():
				first = c
				break
		
		# Find the index of the last digit
		last = None
		for c in line[::-1]:
			if c.isnumeric():
				last = c
				break

		if first is None or last is None:
			raise ValueError("No digits found in the line!")

		total += int(f"{first}{last}")

	return total


### Part 2 ###
LOW_NUMBERS = {
	'one'   :   '1',
	'two'   :   '2',
	'three' :   '3',
	'four'  :   '4',
	'five'  :   '5',
	'six'   :   '6',
	'seven' :   '7',
	'eight' :   '8',
	'nine'  :   '9',
}


def get_num_words(line:str) -> list[tuple[str, iter]]:
	'''Searches the string for all number words, in order,
	 returns a tuple with (word, start index)'''
	
	# Uses lookahead to allow capturing overlapping words,
	# like "eightwo" as both 8 and 2. 
	num_regex = re.compile(fr"(?=({'|'.join(LOW_NUMBERS.keys())}))")
	
	result = []
	for mat in re.finditer(num_regex, line):
		# Tuple of (word, start index)
		result.append((mat.group(1), mat.span()[1]))

	return result


def part_two(source) -> int:
	# - Reads each line of input
	# - Scans the numbers words
	# - Selects either first/last digit or word
	# - Adds the first and last numbers
	# - returns the total sum

	total = 0
	for line in source:
		line = line.strip()

		# Empty line
		if not line: break
		
		words = get_num_words(line)

		# Find the index of the first digit
		first = None
		for i, c in enumerate(line):
			if c.isnumeric():
				first = i
				break
		
		# Find the index of the last digit
		last = None
		for i, c in enumerate(line[::-1]):
			if c.isnumeric():
				last = len(line) - i - 1
				break
		
		# Set digit values based on whether the word or the literal digit occurs first
		if words:
			first = line[first] if first < words[0][1] else LOW_NUMBERS[words[0][0]]
			last = line[last] if last > words[-1][1] else LOW_NUMBERS[words[-1][0]]
		else:
			first = line[first]
			last = line[last]

		if first is None or last is None:
			raise ValueError("No digits found in the line!")

		total += int(f"{first}{last}")

	return total


if __name__ == "__main__":
	if len(sys.argv) <= 1 or sys.argv[1] == '-':
		print("Reading from stdin.")
		text = [ line for line in sys.stdin ]
	else:
		try:
			print(f"Reading from {sys.argv[1]}.")
			with open(sys.argv[1]) as f:
				text = [ line for line in sys.stdin ]
		except FileNotFoundError:
			print(f"File {sys.argv[1]} not found!")
			exit()

	text = list(line for line in sys.stdin)
	print(f"Part 1: {part_one(text)}")
	print(f"Part 2: {part_two(text)}")