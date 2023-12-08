import sys
import re
from math import prod

def parse_game(line:str) -> list[dict]:
	'''Reads the , and ; separted fields into a list of throws'''
	game_separator = re.compile(r"([\w ,]+)(?:;)?")
	cube_separator = re.compile(r"(\d+) (\w+)(?:,;)?")

	game = []
	for turn in re.findall(game_separator, line):
		cubes = {}

		for cube in re.findall(cube_separator, turn):
			cubes[cube[1].lower()] = int(cube[0])

		game.append(cubes)

	return game


def game_possible(rules:dict, game:list[dict]) -> bool:
	'''Applies the rules to the game and returns whether it is possible'''
	for turn in game:
		for cube in turn.keys():
			if not cube in rules: return False
			if turn[cube] > rules[cube]: return False
	
	# Passed without issue
	return True


def minimum_required_cubes(game:list[dict]) -> dict:
	'''Calculates the minimum number of each type of cube required'''
	cubes = {}
	for turn in game:
		for cube in turn.keys():
			if turn[cube] > cubes.get(cube, 0):
				cubes[cube] = turn[cube]
	
	return cubes


def part_one(source) -> int:
	prefix_regex = re.compile(r"^Game (\d+): (.*)$")
	
	rules = {
		"red": 12,
		"green": 13,
		"blue": 14,
	}

	total = 0
	for line in source:
		try:
			mat = re.match(prefix_regex, line)

			num = int(mat.group(1))
			game = mat.group(2)

			if game_possible(rules, parse_game(game)):
				total += num

		except ValueError:
			print("Bad line!")
	
	return total


def part_two(source) -> int:
	prefix_regex = re.compile(r"^Game (\d+): (.*)$")

	total = 0
	for line in source:
		_, game = re.match(prefix_regex, line).groups()

		min_cubes = minimum_required_cubes(parse_game(game))
		power = prod(i for i in min_cubes.values())

		total += power
	
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
	
	print(f"Total - Part 1: {part_one(text)}")
	print(f"Total - Part 2: {part_two(text)}")