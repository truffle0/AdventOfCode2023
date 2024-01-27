from random import choice
from string import ascii_letters, digits
from math import log

### Implementation of a Unique objects type ###

def rng_name(length:int):
    return ''.join(choice(ascii_letters + digits) for _ in range(length))

def unique_name(existing, default_length:int = 16) -> str:
    if len(existing) >= ((len(ascii_letters + digits) ^ default_length) * 0.50):
        # Ensure that there is a decent chance of generating a unique name randomly
        default_length = round(log((len(existing) / 0.5), len(ascii_letter + digits)))

    while name := rng_name(default_length):
        if name not in existing:
            return name

class Unique():
    _instances = {}
    _name = None

    def __new__(cls, *args, **kwargs):
        instances = None
        for c in cls.__mro__:
            if hasattr(c, "_instances") and isinstance(c._instances, dict):
                instances = c._instances
                break
        else:
            raise AttributeError("Failed to locate instance index!")
        
        name = args[0] if args else None
        name = kwargs["name"] if "name" in kwargs else name
        
        if name is None:
            name = unique_name(instances.keys())
        elif not isinstance(name, str):
            raise TypeError("Name argument provided is not a string!")

        if name in instances:
            return instances[name]
        else:
            inst = super().__new__(cls)
            inst._name = name
            instances[name] = inst
            return inst

    @property
    def name(self): return self._name

    def __eq__(self, other):
        # Equality means instances are the same
        return id(self) == id(other)

    def __del__(self):
        self._instances.pop(self._name)
    
    def __hash__(self) -> int:
        return id(self)

    def __repr__(self) -> str:
        return self._name

### Compass Directions Type ###

class Direction(Unique):
    def __neg__(self):
        global NORTH, SOUTH, EAST, WEST
        if self == NORTH:
            return SOUTH
        elif self == SOUTH:
            return NORTH
        elif self == EAST:
            return WEST
        elif self == WEST:
            return EAST

NORTH = Direction("NORTH")
SOUTH = Direction("SOUTH")
EAST = Direction("EAST")
WEST = Direction("WEST")

### Point type ###
class Point:
    def __init__(self, x, y):
        self.x = int(x) if not isinstance(x, int) else x
        self.y = int(y) if not isinstance(y, int) else y
    
    def adjacent(self, other) -> bool:
        x_distance = abs(self.x - other.x)
        y_distance = abs(self.y - other.y)

        if x_distance > 1 or y_distance > 1:
            return False
        
        return x_distance == 0 or y_distance == 0
    
    def direction_to(self, other) -> Direction:
        if not self.adjacent(other):
            raise ValueError("Points are not adjacent!")

        if other.x > self.x:
            return EAST
        elif other.x < self.x:
            return WEST
        elif other.y > self.y:
            return NORTH
        elif other.y < self.y:
            return SOUTH
        
        raise ValueError("Points are not adjacent!")
    
    def __hash__(self) -> int:
        x = self.x
        y = self.y
        # Cantor Pairing function
        return int(((x + y) * (x + y + 1) / 2) + y)
    
    def __repr__(self) -> str:
        return f'({self.x}, {self.y})'
    
    def step(self, dir:Direction):
        if dir == NORTH:
            return Point(self.x, self.y + 1)
        elif dir == SOUTH:
            return Point(self.x, self.y - 1)
        elif dir == EAST:
            return Point(self.x + 1, self.y)
        elif dir == WEST:
            return Point(self.x - 1, self.y)