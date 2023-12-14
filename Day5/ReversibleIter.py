from typing import Sequence, Iterable

class ReversibleIter:
	"Reversible iterator implementation"
	def __init__(self, seq:Sequence):
		self._seq = seq
		self._i = 0
		self._dir = True

	def __iter__(self):
		return self
	
	def __next__(self):
		return self.next()

	def next(self):
		try:
			self._i += 1
			return self._seq[self._i]
		except IndexError:
			self._i = len(self._seq) - 1
			raise StopIteration
	
	def prev(self):
		if self._i - 1 < 0:
			raise StopIteration
		
		self._i -= 1
		return self._seq[self._i]

	def __getitem__(self, index:int):
		try:
			val = self._seq[index]
			self._i = index if index >= 0 else len(self._seq) - index
		except IndexError:
			raise IndexError(f"Index {index} out of bounds!")

	@property
	def start(self): return self._seq[0]
	
	@property
	def end(self): return self._seq[-1]

	@property
	def cur(self): return self._seq[self._i]
	
	def peek(self):
		"Peek at the next element, without iterating"
		try:
			return self._seq[self._i + 1]
		except IndexError:
			return None
	
	def last(self):
		"Return the previous element, without iterating"
		if self._i == 0: return None
		return self._seq[self._i - 1]