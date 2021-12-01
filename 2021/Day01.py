from puzzle_input import puzzle_input
from typing import Iterable


def sum_increases(iterable: Iterable[int]):
    it = iter(iterable)
    count = 0
    first = next(it)
    for num in it:
        if num > first:
            count += 1
        first = num
    return count


if __name__ == "__main__":
    PUZZLE_INPUT = [int(i) for i in puzzle_input().splitlines()]

    # Part 1
    print("Part 1:", sum_increases(PUZZLE_INPUT))

    # Part 2
    groups: list[int] = []
    activegroups = [PUZZLE_INPUT[0] + PUZZLE_INPUT[1], PUZZLE_INPUT[1], 0]
    for num in PUZZLE_INPUT[2:]:
        for i in range(3):
            activegroups[i] += num
        groups.append(activegroups.pop(0))
        activegroups.append(0)
    print("Part 2:", sum_increases(groups))
