import sys
import re
from typing import Generator
from enum import Enum

from mytypes import Direction, NORTH, SOUTH, EAST, WEST, Point

"""
	I chose to play around alot with classes in this one
	It probably made it significantly less efficient, yes, but it was fun to do.
"""

class Path:
	_possible_tiles = {
		"|": {NORTH, SOUTH},
		"-": {EAST, WEST},
		"L": {NORTH, EAST},
		"J": {NORTH, WEST},
		"7": {SOUTH, WEST},
		"F": {SOUTH, EAST},
		".": None,
		"S": None,
	}

	def __init__(self, type:str, pos:Point):
		self._pos = pos
		self._peers = {}
		
		if str in self._possible_tiles:
			self._tile = type
		else:
			raise ValueError("Invalid tile")

	@property
	def pos(self) -> Point:
		return self._pos
	
	@property
	def tile(self) -> str:
		return self._tile

	def __getattr__(self, name:str):
		return self.__getitem__(name)

	def __getitem__(self, key):
		try:
			if isinstance(key, Direction):
				return self._peers[key]
			elif isinstance(key, str):
				match key.lower():
					case 'n' | 'north':
						return self._peers[NORTH]
					case 'e' | 'east':
						return self._peers[EAST]
					case 's' | 'south':
						return self._peers[SOUTH]
					case 'w' | 'west':
						return self._peers[WEST]
		except AttributeError:
			return None

	def __setitem__(self, key, value):
		if isinstance(key, Direction):
			self._peers[key] = value
		elif isinstance(key, str):
			match key.lower():
				case 'n' | 'north':
					self._peers[NORTH] = value
				case 'e' | 'east':
					self._peers[EAST] = value
				case 's' | 'south':
					self._peers[SOUTH] = value
				case 'w' | 'west':
					self._peers[WEST] = value
		else:
			raise TypeError("Invalid attribute type!")

	def direction_to(self, other) -> Direction:
		return self.pos.direction_to(other.pos)

	def link(self, other):
		if self.pos.adjacent(other.pos):
			dir = self.direction_to(other)
			
			self[dir] = other
			other[-dir] = self

	@property
	def options(self) -> set[Direction]:
		return self._possible_tiles[self._tile]

def findloop(source):
	...

def part_one(source):
	pass

if __name__ == '__main__':
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
	
	maze = Maze(text)
	
	