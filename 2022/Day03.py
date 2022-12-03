import string

from puzzle_input import puzzle_input


def get_compartments(line: str) -> tuple[set[str], set[str]]:
    half = len(line) // 2
    return set(line[:half]), set(line[half:])


ELEMENT_PRIORITIES = "_" + string.ascii_lowercase + string.ascii_uppercase


def element_priority(c: str) -> int:
    assert len(c) == 1
    return ELEMENT_PRIORITIES.index(c)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    part1 = sum(
        element_priority(next(iter(a & b)))
        for a, b in map(get_compartments, PUZZLE_INPUT)
    )
    print(f"Part 1: {part1}")

    TRIPLES = [PUZZLE_INPUT[i : i + 3] for i in range(0, len(PUZZLE_INPUT), 3)]
    part2 = sum(
        element_priority(next(iter(a & b & c)))
        for a, b, c in ((set(line) for line in triple) for triple in TRIPLES)
    )
    print(f"Part 2: {part2}")
