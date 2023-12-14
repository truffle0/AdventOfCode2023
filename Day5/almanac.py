import re
import sys
import logging
from typing import Generator
from tqdm import tqdm
from itertools import chain, cycle

class TypeMap:
	def __init__(self, text:str):
		'''Expects the format:
			x-to-y map:
			x-start y-start range
			...
		'''

		# Match title
		re_title = re.compile(r"^(\w+)-to-(\w+) map:.*")

		match = re.match(re_title, text)
		if match is None:
			raise ValueError("Unrecognised format!")
		self.input = match.group(1)
		self.output = match.group(2)

		# Find and store mappings
		self._mappings:dict[range, int] = {}

		re_mapping = re.compile(r"^(\d+) (\d+) (\d+)$", re.MULTILINE)
		for match in re.findall(re_mapping, text):
			if match is None:
				break
			
			x = int(match[1])
			y = int(match[0])
			offset = int(match[2])

			# Associate the range with the conversion factor/offset
			map_range = range(x, x + offset)
			map_convert = y - x
			self._mappings[map_range] = map_convert

	def converter(self):
		"""Return a generator that performs mapping efficiently in a stream"""
		def generator() -> Generator[int, int, None]:
			# Infinitely loop over the mappings, returning the mappings
			i = yield 0
			while True:
				for map_range, offset in self._mappings.items():
					if i in map_range:
						while i in map_range:
							i = yield i + offset
						break
				else:
					i = yield i
		
		gen = generator()
		next(gen)	# Initalize the generator
		return gen

	def __getitem__(self, index):
		for map_range, offset in self._mappings.items():
			if index in map_range:
				return index + offset
		else:
			return index
	
	def __repr__(self) -> str:
		string = f"{self.input}-to-{self.output} map:\n"
		for map_in, map_offset in self._mappings.items():
			x = map_in.start
			y = x + map_offset
			offset = len(map_in)

			string += f"{x} {y} {offset}\n"

		return string


class Almanac:
	def __init__(self):
		# Dictionary containing all associations
		# As a mapping of the input : (output, TypeMap)
		# Where input/output are strings naming the types
		# and TypeMap is the TypeMap object that converts them
		self._alltypes:set[str] = set()
		self._associations:dict[str,dict[str,TypeMap]] = dict()
	
	def extend(self, assoc:TypeMap) -> None:
		# Add to list of available types
		self._alltypes.add(assoc.input)
		self._alltypes.add(assoc.output)

		# Add the association to the dictionary
		if assoc.input not in self._associations:
			self._associations[assoc.input] = dict()

		self._associations[assoc.input][assoc.output] = assoc
		logging.debug(f"Registered TypeMap: {assoc.input} -> {assoc.output}")

	def search_path(self, from_type:str, to_type:str) -> list[str]:
		"""Generates the shortest possible path to convert
			one type to another."""
		pass
		
	def convert(self, val:int, from_type:str, to_type:str) -> int:
		if to_type in self[from_type]:
			return self._associations[from_type][to_type][val]
		else:
			raise ValueError(f"No mapping from {from_type} to {to_type} available!")


	def convert_path(self, val:int, conv_path:list[str]) -> int:
		"""Takes a list of types (as strings) and follows that
			order of transformations, returning the result"""
		prev_type = None
		for next_type in conv_path:
			if not prev_type:
				prev_type = next_type
			else:
				val = self.convert(val, prev_type, next_type)
				prev_type = next_type
		
		return val

	def path_converter(self, path:list[str]):
		"""Creates a generator that efficiently maps streams of ints"""
		steps = []
		prev_type = path[0]
		for next_type in path[1:]:
			steps.append(self._associations[prev_type][next_type].converter())
			prev_type = next_type
		
		def generator() -> Generator[int, int, None]:
			i = yield 0
			conversion = steps
			while True:
				if isinstance(i, int):
					for gen in conversion:
						i = gen.send(i)
					i = yield i
				else:
					i = yield 0

		# Create and initialize generator
		gen = generator()
		next(gen)
		return gen

	def __getitem__(self, item) -> list[str]:
		if item not in self._alltypes:
			raise NameError("No such type!")

		if item in self._associations:
			return list(self._associations[item].keys())
		
		return []
		
	def __contains__(self, item) -> bool:
		return item in self._alltypes


def parse_config(source:str) -> tuple[list[int],Almanac,list[str]]:
	"""Parses almanac config.
		Returns a list of seed numbers, dictionary of available maps and the order the appear"""
	re_seeds = r"^seeds:(?:\s\d+)+"
	re_map = r"^(?:\w+)-to-(?:\w+) map:\n(?:\d+ \d+ \d+\n)*"
	re_block = re.compile(fr"({re_seeds})|({re_map})", re.MULTILINE)

	seeds = []
	maps = Almanac()
	conv_order = ["seed"]
	for m_seeds, m_map in re.findall(re_block, source):
		if len(m_seeds):
			seeds.extend([int(i) for i in re.findall(r"\d+", m_seeds)])
		else:
			tmap = TypeMap(m_map)
			maps.extend(tmap)
			conv_order.append(tmap.output)
	
	return seeds, maps, conv_order


def group(iterable, n):
	data = iter(iterable)
	elements = []

	for elem in data:
		elements.append(elem)
		if len(elements) >= n:
			yield tuple(elements)
			elements.clear()
	
	if elements:
		elements.extend([None] * (n - len(elements)))
		yield tuple(elements)


def part_one(source:str) -> int:
	seeds, almanac, conversion = parse_config(source)
	logging.debug(f"Using mapping: {'->'.join(conversion)}")

	locations = [ almanac.convert_path(seed, conversion) for seed in seeds ]

	return min(locations)

def part_two(source:str) -> int:
	"""Fair warning: this function is horribly inefficient
		Requiring roughly 2 billion iterations to cover the entire range"""
	seeds, almanac, conversion = parse_config(source)
	
	# interpret pairs in the list as ranges
	ranges = []
	for start, length in group(seeds, 2):
		ranges.append(range(start, start + length))
	
	# Create converter
	converter = almanac.path_converter(conversion)
	next(converter)	# Initialize generator

	total = sum(len(ran) for ran in ranges)
	result = min(converter.send(i)
		for i in tqdm(chain(*ranges), total=total, desc="Maps"))

	# Return the minimum value
	return result

if __name__ == "__main__":
	logging.basicConfig(level=logging.INFO)
	if len(sys.argv) <= 1 or sys.argv[1] == '-':
		print("Reading from stdin.")
		text = sys.stdin.read()
	else:
		try:
			print(f"Reading from {sys.argv[1]}")
			with open(sys.argv[1]) as f:
				text = f.read()
		except FileNotFoundError:
			print(f"File {sys.argv[1]} not found!")
			exit()
	
	print(f"Part 1: {part_one(text)}")
	print(f"Part 2: {part_two(text)}")