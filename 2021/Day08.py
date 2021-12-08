from collections import defaultdict
from typing import Iterable, TypeVar
from puzzle_input import puzzle_input


T = TypeVar("T")


def get_only(it: Iterable[T]) -> T:
    iterator = iter(it)
    value = next(iterator)
    try:
        next(iterator)
    except StopIteration:
        return value
    else:
        raise ValueError("Multiple values")


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
UNIQUESEGMENTCOUNTS = {k: get_only(v) for k, v in SEGMENTCOUNTS.items() if len(v) == 1}
# SINGLES = {
#     (a, b): get_only(s)
#     for a in SEVENSEGMENTS
#     for b in SEVENSEGMENTS
#     if len(s := SEVENSEGMENTS[a] - SEVENSEGMENTS[b]) == 1
# }
# SEGMENTOCCURANCES = {c: {i for i, s in SEVENSEGMENTS.items() if c in s} for c in ALLSET}


def map_seven(uniques: Iterable[frozenset[str]]) -> dict[str, str]:
    uniques = set(uniques)

    by_length: defaultdict[int, set[str]] = defaultdict(set)
    for s in uniques:
        by_length[len(s)].update(s)

    A = get_only(by_length[3] - by_length[2])

    by_occurance: defaultdict[str, int] = defaultdict(lambda: 0)
    for s in uniques:
        for c in s:
            by_occurance[c] += 1
    r_by_occurance = {v: k for k, v in by_occurance.items() if k != A}

    B = get_only(r_by_occurance[6])
    C = get_only(r_by_occurance[8])  # except for A
    E = get_only(r_by_occurance[4])
    F = get_only(r_by_occurance[9])
    known = {A, B, C, E, F}
    ZERO = get_only(
        u for u in uniques if known.issubset(u) and len(u) == len(known) + 1
    )
    G = get_only(ZERO - known)
    D = get_only(ALLSET - known - {G})
    return {"a": A, "b": B, "c": C, "d": D, "e": E, "f": F, "g": G}


def get_sevens(mapping: dict[str, str]) -> dict[frozenset[str], int]:
    return {frozenset(mapping[c] for c in v): k for k, v in SEVENSEGMENTS.items()}


if __name__ == "__main__":
    PUZZLE_INPUT = [
        tuple([frozenset(p) for p in part.split()] for part in line.split(" | "))
        for line in puzzle_input().splitlines()
    ]

    part1 = 0
    part2 = 0
    for u, o in PUZZLE_INPUT:
        m = get_sevens(map_seven(u))
        part2partial = 0
        for p in o:
            if len(p) in UNIQUESEGMENTCOUNTS:
                part1 += 1
            part2partial = part2partial * 10 + m[p]
        part2 += part2partial
    print("Part 1:", part1)
    print("Part 2:", part2)
