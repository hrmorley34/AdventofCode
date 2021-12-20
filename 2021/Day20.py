from __future__ import annotations

from Day17 import Vector  # I can't be bothered
from functools import reduce
from puzzle_input import puzzle_input
from typing import Iterable, Iterator, Mapping, Sequence


class InfiniteError(Exception):
    pass


LIGHT_PIXEL = "#"
DARK_PIXEL = "."


class Grid(Mapping[Vector, bool]):
    _set: frozenset[Vector]

    def __init__(self, s: Iterable[Vector]) -> None:
        self._set = frozenset(s)

    def __getitem__(self, key: Vector) -> bool:
        return key in self._set

    def __iter__(self) -> Iterator[Vector]:
        return self._set.__iter__()

    def __len__(self) -> int:
        return len(self._set)

    def count_lit(self) -> int:
        return len(self._set)

    @classmethod
    def from_string(cls, lines: Iterable[str]) -> Grid:
        s: set[Vector] = set()
        for y, line in enumerate(lines):
            for x, c in enumerate(line):
                if c == LIGHT_PIXEL:
                    s.add(Vector(x, y))
        return cls(s)

    def borders(self) -> tuple[tuple[int, int], tuple[int, int]]:
        return (
            (min(v.x for v in self._set), min(v.y for v in self._set)),
            (max(v.x for v in self._set), max(v.y for v in self._set)),
        )

    def printable(self) -> list[str]:
        (minx, miny), (maxx, maxy) = self.borders()

        lines: list[str] = []
        for y in range(miny, maxy + 1):
            line = ""
            for x in range(minx, maxx + 1):
                line += LIGHT_PIXEL if self[Vector(x, y)] else DARK_PIXEL
            lines.append(line)
        return lines

    def __repr__(self) -> str:
        if len(self._set) < 1000:
            return "\n".join(self.printable())
        else:
            return super().__repr__()


class AntiGrid(Grid):
    _set: frozenset[Vector]

    def __init__(self, s: Iterable[Vector]) -> None:
        self._set = frozenset(s)

    def __getitem__(self, key: Vector) -> bool:
        return key not in self._set

    def __iter__(self) -> Iterator[Vector]:
        raise InfiniteError

    def __len__(self) -> int:
        raise InfiniteError

    def count_lit(self) -> int:
        raise InfiniteError

    @classmethod
    def from_string(cls, lines: Iterable[str]) -> AntiGrid:
        raise NotImplementedError

    def borders(self) -> tuple[tuple[int, int], tuple[int, int]]:
        return (
            (min(v.x for v in self._set), min(v.y for v in self._set)),
            (max(v.x for v in self._set), max(v.y for v in self._set)),
        )


NEIGHBOURS_AND_THIS = tuple(Vector(x, y) for y in range(-1, 2) for x in range(-1, 2))


def enhance(grid: Grid, enhancer: Sequence[bool]) -> Grid:
    if enhancer[0]:
        assert not enhancer[0b111_111_111]
    is_anti = enhancer[0] ^ isinstance(grid, AntiGrid)

    (minx, miny), (maxx, maxy) = grid.borders()

    new_set: set[Vector] = set()
    for y in range(miny - 1, maxy + 2):
        for x in range(minx - 1, maxx + 2):
            v = Vector(x, y)
            vecs = (v + adj for adj in NEIGHBOURS_AND_THIS)
            bools = (grid[v] for v in vecs)
            boolint = reduce(lambda a, b: a * 2 + int(b), bools, 0)

            if enhancer[boolint] ^ is_anti:
                new_set.add(v)

    cls = AntiGrid if is_anti else Grid
    return cls(new_set)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    ENHANCER, IMAGE = PUZZLE_INPUT.split("\n\n")
    BOOL_ENHANCER = [c == LIGHT_PIXEL for c in ENHANCER]
    GRID = Grid.from_string(IMAGE.splitlines())

    grid = GRID
    for _ in range(2):
        grid = enhance(grid, BOOL_ENHANCER)
    print("Part 1:", grid.count_lit())

    for _ in range(2, 50):
        grid = enhance(grid, BOOL_ENHANCER)
    print("Part 2:", grid.count_lit())
