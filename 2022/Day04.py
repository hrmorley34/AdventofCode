from puzzle_input import puzzle_input


RANGE_TYPE = tuple[tuple[int, int], tuple[int, int]]


def parse_line(line: str) -> RANGE_TYPE:
    a, b = line.split(",")
    ami, ama = a.split("-")
    bmi, bma = b.split("-")
    return (int(ami), int(ama)), (int(bmi), int(bma))


def one_in_other(line: RANGE_TYPE) -> bool:
    (ami, ama), (bmi, bma) = line
    return (ami <= bmi and ama >= bma) or (ami >= bmi and ama <= bma)


def any_overlap(line: RANGE_TYPE) -> bool:
    (ami, ama), (bmi, bma) = line
    return not (ami > bma or ama < bmi)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    RANGES = [parse_line(line) for line in PUZZLE_INPUT]

    part1 = len([r for r in RANGES if one_in_other(r)])
    print(f"Part 1: {part1}")

    part2 = len([r for r in RANGES if any_overlap(r)])
    print(f"Part 2: {part2}")
