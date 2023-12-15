import re
from sys import argv, stdin
from math import prod

def win_races(time:int, record:int) -> int:
    # charge = time spent charging = speed of the boat
    # charge - time = remaining time
    distances = [ (time - charge) * charge for charge in range(0, time) ]

    # collect elements that pass the record and return the number
    return sum(x > record for x in distances)

def parse_config(source) -> list[tuple[int, int]]:
    re_time = re.compile(r"^Time:\s+((?:\d+\s*)+)", re.MULTILINE)
    re_distance = re.compile(r"^Distance:\s+((?:\d+\s*)+)", re.MULTILINE)
    re_int = re.compile(r"(\d+)")

    times = re.findall(re_int, ' '.join(re.findall(re_time, source)))
    distances = re.findall(re_int, ' '.join(re.findall(re_distance, source)))
    
    races = zip(times, distances)
    return list(races)

def part_one(races: list[tuple[int, int]]) -> int:
    return prod(win_races(int(time), int(record)) for time, record in races)

def part_two(races: list[tuple[int, int]]) -> int:
    # Unpack tuples in the list, assign them to variables,
    # concatenate into a single string, then convert to an int
    time, record = zip(*races)
    time = int(''.join(time))
    record = int(''.join(record))

    return win_races(time, record)

if __name__ == "__main__":
    if len(argv) < 2 or argv[1] == '-':
        print(f"Reading from stdin")
        text = stdin.read()
    else:
        try:
            with open(argv[1]) as f:
                print(f"Reading from {argv[1]}")
                text = f.read()
        except FileNotFoundError:
            print(f"Unable to find file: {argv[1]}")
            exit(1)
    
    races = parse_config(text)

    print(f"Part 1: {part_one(races)}")
    print(f"Part 2: {part_two(races)}")