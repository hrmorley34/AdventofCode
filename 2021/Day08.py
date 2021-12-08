from collections import defaultdict
from puzzle_input import puzzle_input


ALLSET = frozenset("abcdefg")
SEVENSEGMENTS = {
    0: frozenset("abcefg"),
    1: frozenset("cf"),
    2: frozenset("acdeg"),
    3: frozenset("acdfg"),
    4: frozenset("bcdf"),
    5: frozenset("abdfg"),
    6: frozenset("abdefg"),
    7: frozenset("acf"),
    8: frozenset("abcdefg"),
    9: frozenset("abcdfg"),
}
REVERSESEVENSEGMENTS = {v: k for k, v in SEVENSEGMENTS.items()}
SEGMENTCOUNTS: defaultdict[int, set[int]] = defaultdict(set)
for k, v in SEVENSEGMENTS.items():
    SEGMENTCOUNTS[len(v)].add(k)
UNIQUESEGMENTCOUNTS = {
    k: next(iter(v)) for k, v in SEGMENTCOUNTS.items() if len(v) == 1
}


if __name__ == "__main__":
    PUZZLE_INPUT = [
        tuple([frozenset(p) for p in part.split()] for part in line.split(" | "))
        for line in puzzle_input().splitlines()
    ]
    part1 = 0
    for u, o in PUZZLE_INPUT:
        for p in o:
            if len(p) in UNIQUESEGMENTCOUNTS:
                part1 += 1
    print("Part 1:", part1)
