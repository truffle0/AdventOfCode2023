import sys
import re

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


if __name__ == "__main__":
	prefix_regex = re.compile(r"^Game (\d+): (.*)$")
	
	rules = {
		"red": 12,
		"green": 13,
		"blue": 14,
	}

	total = 0
	for line in sys.stdin:
		try:
			mat = re.match(prefix_regex, line)

			num = int(mat.group(1))
			game = mat.group(2)

			if game_possible(rules, parse_game(game)):
				total += num

		except ValueError:
			print("Bad line!")
	
	print(f"Total: {total}")