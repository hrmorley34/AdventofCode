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


def polymerise_by_neighbour(
    polymer: Counter[tuple[PolymerPart, PolymerPart]],
    mapping: dict[tuple[PolymerPart, PolymerPart], PolymerPart],
) -> Counter[tuple[PolymerPart, PolymerPart]]:
    new_polymer: Counter[tuple[PolymerPart, PolymerPart]] = Counter()
    for (keyl, keyr), value in polymer.items():
        keymiddle = mapping[keyl, keyr]
        new_polymer[keyl, keymiddle] += value
        new_polymer[keymiddle, keyr] += value
    return new_polymer


def polymer_neighbour_to_count(
    polymer: Counter[tuple[PolymerPart, PolymerPart]],
    start: PolymerPart,
    end: PolymerPart,
) -> Counter[PolymerPart]:
    c: Counter[PolymerPart] = Counter((start, end))
    for key in polymer:
        for p in key:
            c[p] += polymer[key]
    return Counter({k: v // 2 for k, v in c.items()})


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    basepolymer, lines = PUZZLE_INPUT.split("\n\n")
    POLYMER = list(map(PolymerPart, basepolymer))
    MAPPING = {
        (PolymerPart(a[0]), PolymerPart(a[1])): PolymerPart(b)
        for a, b in map(lambda s: s.split(" -> "), lines.splitlines())
    }
    POLYMERCOUNT = Counter(
        (POLYMER[x], POLYMER[x + 1]) for x in range(len(POLYMER) - 1)
    )

    # polymer = POLYMER
    # for _ in range(10):
    #     polymer = polymerise(polymer, MAPPING)
    # counts = Counter(polymer)

    polymer = POLYMERCOUNT
    for _ in range(10):
        polymer = polymerise_by_neighbour(polymer, MAPPING)
    counts = polymer_neighbour_to_count(polymer, POLYMER[0], POLYMER[-1])
    order = counts.most_common()
    print("Part 1:", order[0][1] - order[-1][1])

    for _ in range(40 - 10):
        polymer = polymerise_by_neighbour(polymer, MAPPING)
    counts = polymer_neighbour_to_count(polymer, POLYMER[0], POLYMER[-1])
    order = counts.most_common()
    print("Part 2:", order[0][1] - order[-1][1])
