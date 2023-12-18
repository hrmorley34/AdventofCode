import re
from typing import Literal, NamedTuple, cast

from Day10 import fill_sides
from puzzle_input import puzzle_input

DIR_TUPLES = {"U": (0, -1), "D": (0, 1), "L": (-1, 0), "R": (1, 0)}


class DigPlanLine(NamedTuple):
    dir: Literal["U", "D", "L", "R"]
    length: int
    colour: str

    @classmethod
    def from_line(cls, line: str):
        r = re.match(r"([UDLR]) (\d+) \(#(\w{6})\)", line)
        assert r is not None, line
        return cls(cast(Literal["U", "D", "L", "R"], r[1]), int(r[2]), r[3])

    @property
    def dir_tuple(self) -> tuple[int, int]:
        return DIR_TUPLES[self.dir]


def create_zero_aligned_edge(
    lines: list[DigPlanLine]
) -> tuple[
    tuple[int, int],
    dict[tuple[int, int], tuple[int, int]],
    tuple[int, int],
]:
    cx, cy = (0, 0)
    maps: dict[tuple[int, int], tuple[int, int]] = {}
    minx, miny, maxx, maxy = 0, 0, 0, 0

    for line in lines:
        dx, dy = line.dir_tuple
        for _ in range(line.length):
            newx, newy = cx + dx, cy + dy
            maps[cx, cy] = (newx, newy)
            (cx, cy) = (newx, newy)
        minx, miny = min(minx, cx), min(miny, cy)
        maxx, maxy = max(maxx, cx), max(maxy, cy)
    assert (cx, cy) == (0, 0)

    return (
        (-minx, -miny),
        {
            (f[0] - minx, f[1] - miny): (t[0] - minx, t[1] - miny)
            for f, t in maps.items()
        },
        (maxx - minx + 1, maxy - miny + 1),
    )


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    lines = list(map(DigPlanLine.from_line, PUZZLE_INPUT))
    start, conn, dim = create_zero_aligned_edge(lines)
    area = fill_sides(start, conn, dim) + len(conn)
    print("Part 1:", area)
