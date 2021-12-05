from __future__ import annotations

from collections import defaultdict
from dataclasses import dataclass
from puzzle_input import puzzle_input
from typing import Any


@dataclass(frozen=True)
class Position:
    x: int
    y: int

    def __add__(self, p: Position | Any) -> Position:
        if isinstance(p, Position):
            return Position(self.x + p.x, self.y + p.y)
        return NotImplemented

    def __neg__(self) -> Position:
        return Position(-self.x, -self.y)

    def __sub__(self, p: Position | Any) -> Position:
        if isinstance(p, Position):
            return self + (-p)
        return NotImplemented


class Vent:
    ends: tuple[Position, Position]

    def __init__(self, start: Position, end: Position):
        self.ends = (start, end)

    def __repr__(self) -> str:
        return "Vent" + repr(self.ends)

    @property
    def on_axis(self) -> tuple[bool, bool]:
        return (self.ends[0].x == self.ends[1].x, self.ends[0].y == self.ends[1].y)

    @property
    def on_diagonal(self) -> tuple[bool, bool]:
        if self.ends[0].y - self.ends[1].y == 0:
            return (False, False)
        div = (self.ends[0].x - self.ends[1].x) / (self.ends[0].y - self.ends[1].y)
        return (div == 1, div == -1)

    @property
    def borders(self) -> tuple[tuple[int, int], tuple[int, int]]:
        return (
            (min(p.x for p in self.ends), min(p.y for p in self.ends)),
            (max(p.x for p in self.ends), max(p.y for p in self.ends)),
        )

    def covers(self) -> set[Position]:
        xa, ya = self.on_axis
        fdiag, bdiag = self.on_diagonal
        if not (xa or ya or fdiag or bdiag):
            raise NotImplementedError
        if xa and ya:
            return set(self.ends)

        (minx, miny), (maxx, maxy) = self.borders

        s: set[Position] = set()
        example = next(iter(self.ends))
        if xa:
            for y in range(miny, maxy + 1):
                s.add(Position(example.x, y))
        elif ya:
            for x in range(minx, maxx + 1):
                s.add(Position(x, example.y))
        elif fdiag:
            for d in range(0, maxx - minx + 1):
                s.add(Position(minx + d, miny + d))
        elif bdiag:
            for d in range(0, maxx - minx + 1):
                s.add(Position(minx + d, maxy - d))
        return s


if __name__ == "__main__":
    PUZZLE_INPUT = [
        Vent(*(Position(*map(int, part.split(","))) for part in line.split(" -> ")))
        for line in puzzle_input().splitlines()
        # for line in clipboard.splitlines()
    ]
    boundaries = (
        (
            min(p.x for v in PUZZLE_INPUT for p in v.ends),
            min(p.y for v in PUZZLE_INPUT for p in v.ends),
        ),
        (
            max(p.x for v in PUZZLE_INPUT for p in v.ends),
            max(p.y for v in PUZZLE_INPUT for p in v.ends),
        ),
    )
    SPACE: defaultdict[Position, int] = defaultdict(lambda: 0)
    for v in PUZZLE_INPUT:
        if not any(v.on_axis):
            continue
        for p in v.covers():
            SPACE[p] += 1

    print("Part 1:", len([c for c, i in SPACE.items() if i >= 2]))

    SPACE2: defaultdict[Position, int] = defaultdict(lambda: 0)
    for v in PUZZLE_INPUT:
        if not (any(v.on_axis) or any(v.on_diagonal)):
            continue
        for p in v.covers():
            SPACE2[p] += 1

    print("Part 2:", len([c for c, i in SPACE2.items() if i >= 2]))
