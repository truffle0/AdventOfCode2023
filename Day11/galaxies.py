import sys
from itertools import combinations

def expand_universe(starmap:list[list]):
	"""Expand the 'universe' on all empty rows & columns that don't contain galaxies"""
	newmap = []
	for row in starmap:
		if '#' not in row:
			newmap.append(list(row))
		
		newmap.append(list(row))
	
	adjust = 0
	for i in range(len(starmap[0])):
		col = [ row[i] for row in starmap ]

		if '#' not in col:
			for row in newmap:
				row.insert(i + adjust, '.')
			
			adjust += 1

	return newmap

class Point:
	"""Immutable point type.
		A tuple would have also been fine in place, this is just for practice"""

	def __init__(self, x, y):
		if not isinstance(x, int) or not isinstance(y, int):
			raise TypeError("Input not an int!")

		self.x = x
		self.y = y

	def __setattr__(self, name, value):
		if name in ("x", "y") and name in self.__dict__:
			raise ValueError(f"{name} field is immutable!")

		super(Point, self).__setattr__(name, value)

	def __hash__(self) -> int:
		# Cantor Pairing function
		return int((self.x + self.y) * (self.x + self.y + 1) / 2 + self.y)
	
	def __eq__(self, other) -> bool:
		return self.x == other.x and self.y == other.y
	
	def stepsto(self, other) -> int:
		return abs(other.x - self.x) + abs(other.y - self.y)
	
	def __repr__(self) -> str:
		return f"({self.x},{self.y})"


def part_one(source):
	starmap = [ list(line) for line in source ]
	
	expanded = expand_universe(starmap)

	galaxies = []
	for y, row in enumerate(expanded):
		for x, point in enumerate(row):
			if point == '#':
				galaxies.append(Point(x, y))
	
	pairs = combinations(galaxies, 2)
	sum = 0
	for a, b in pairs:
		sum += a.stepsto(b)
	
	return sum

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
		
	print(f"Part 1: {part_one(text)}")