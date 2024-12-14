import re
from collections import Counter
from dataclasses import dataclass
from math import prod
from typing import Self

from puzzle_input import puzzle_input

# SPACE = (11, 7)
SPACE = (101, 103)
TIME = 100

RE_ROBOT = re.compile(r"p=(\d+),(\d+) v=(-?\d+),(-?\d+)")


@dataclass
class Robot:
    p: tuple[int, int]
    v: tuple[int, int]

    def step(self, bounds: tuple[int, int], i: int = 1) -> None:
        self.p = (
            (self.p[0] + self.v[0] * i) % bounds[0],
            (self.p[1] + self.v[1] * i) % bounds[1],
        )

    @classmethod
    def from_line(cls, line: str) -> Self:
        m = RE_ROBOT.fullmatch(line.strip())
        assert m is not None
        return cls((int(m[1]), int(m[2])), (int(m[3]), int(m[4])))


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    ROBOTS = list(map(Robot.from_line, PUZZLE_INPUT))

    quadrants: Counter[tuple[bool, bool]] = Counter()
    for r in ROBOTS:
        r.step(SPACE, TIME)
        if r.p[0] != SPACE[0] // 2 and r.p[1] != SPACE[1] // 2:
            quadrants[r.p[0] > SPACE[0] // 2, r.p[1] > SPACE[1] // 2] += 1
    print("Part 1:", prod(quadrants.values()) if len(quadrants) >= 4 else 0)
