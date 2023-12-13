from operator import itemgetter
from typing import Sequence

from puzzle_input import puzzle_input


def is_sim_diff1(line1: Sequence[str], line2: Sequence[str]) -> bool:
    assert len(line1) == len(line2)
    return sum(c1 == c2 for c1, c2 in zip(line1, line2)) == len(line1) - 1


def find_refl_hor(block: list[str]) -> tuple[int, int]:
    p1 = p2 = 0  # default to no reflection
    for i in range(1, len(block)):
        matches = [
            block[i - y - 1] == block[i + y] for y in range(i) if i + y < len(block)
        ]
        if all(matches):
            p1 = i
        elif matches.count(False) == 1:
            bad_y = matches.index(False)
            if is_sim_diff1(block[i - bad_y - 1], block[i + bad_y]):
                p2 = i
    return (p1, p2)


def find_refl_ver(block: list[str]) -> tuple[int, int]:
    p1 = p2 = 0  # default to no reflection
    for i in range(len(block[0]) - 1, 0, -1):
        matches = [
            list(map(itemgetter(i - x - 1), block))
            == list(map(itemgetter(i + x), block))
            for x in range(i)
            if i + x < len(block[0])
        ]
        if all(matches):
            p1 = i
        elif matches.count(False) == 1:
            bad_x = matches.index(False)
            if is_sim_diff1(
                list(map(itemgetter(i - bad_x - 1), block)),
                list(map(itemgetter(i + bad_x), block)),
            ):
                p2 = i
    return (p1, p2)


def sum_reflections(blocks: list[list[str]]) -> tuple[int, int]:
    reflections = [(find_refl_hor(block), find_refl_ver(block)) for block in blocks]
    return (
        sum(100 * r[0][0] + r[1][0] for r in reflections),
        sum(100 * r[0][1] + r[1][1] for r in reflections),
    )


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().split("\n\n")
    blocks = [block.splitlines() for block in PUZZLE_INPUT]

    p1, p2 = sum_reflections(blocks)
    print("Part 1:", p1)
    print("Part 2:", p2)
