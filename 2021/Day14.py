from collections import Counter
from puzzle_input import puzzle_input
from typing import NewType


PolymerPart = NewType("PolymerPart", str)


def polymerise(
    polymer: list[PolymerPart],
    mapping: dict[tuple[PolymerPart, PolymerPart], PolymerPart],
) -> list[PolymerPart]:
    polymer = polymer.copy()
    for i in reversed(range(1, len(polymer))):
        polymer.insert(i, mapping[polymer[i - 1], polymer[i]])
    return polymer


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    basepolymer, lines = PUZZLE_INPUT.split("\n\n")
    POLYMER = list(map(PolymerPart, basepolymer))
    MAPPING = {
        (PolymerPart(a[0]), PolymerPart(a[1])): PolymerPart(b)
        for a, b in map(lambda s: s.split(" -> "), lines.splitlines())
    }
    polymer = POLYMER
    for _ in range(10):
        polymer = polymerise(polymer, MAPPING)
    counts = Counter(polymer)
    order = counts.most_common()
    print("Part 1:", order[0][1] - order[-1][1])
