from enum import Enum
from itertools import pairwise
from typing import Iterable

from puzzle_input import puzzle_input


def get_lines(s: str):
    for line in s.splitlines():
        yield from pairwise(
            (a, b) for a, b in map(lambda s: map(int, s.split(",")), line.split(" -> "))
        )


class Material(Enum):
    AIR = 0
    ROCK = 1
    SAND = 2

    def to_char(self) -> str:
        if self == self.AIR:
            return "."
        elif self == self.ROCK:
            return "#"
        elif self == self.SAND:
            return "o"
        else:
            raise Exception(f"Unknown material: {self}")


class Grid:
    grid: dict[tuple[int, int], Material]
    sand_gen: tuple[int, int]
    range: tuple[int, int, int, int]
    abyss: int
    floor: int
    sand_count: int

    def __init__(
        self,
        lines: Iterable[tuple[tuple[int, int], tuple[int, int]]],
        sand_gen: tuple[int, int],
    ) -> None:
        self.grid = {}
        MIN_Y = sand_gen[1]
        max_y = 0
        min_x = +10_000
        max_x = -10_000
        for (a, b), (i, j) in lines:
            if a == i:
                b, j = sorted((b, j))
                for y in range(b, j + 1):
                    self.grid[a, y] = Material.ROCK
            elif b == j:
                a, i = sorted((a, i))
                for x in range(a, i + 1):
                    self.grid[x, b] = Material.ROCK
            else:
                raise Exception(f"Non-straight line: ({a}, {b}) -> ({i}, {j})")
            max_y = max(max_y, b, j)
            min_x = min(min_x, a, i)
            max_x = max(max_x, a, i)
        self.sand_gen = sand_gen
        self.range = (min_x, MIN_Y, max_x, max_y)
        self.abyss = max_y + 1
        self.floor = max_y + 2
        self.sand_count = 0

    def _try_move(
        self, from_: tuple[int, int], to: tuple[int, int]
    ) -> tuple[int, int] | None:
        assert from_ == self.sand_gen or self.grid.get(from_) == Material.SAND
        if self.grid.get(to, Material.AIR) == Material.AIR:
            if self.grid.get(from_) == Material.SAND:
                # self.grid[from_] = Material.AIR
                del self.grid[from_]
            self.grid[to] = Material.SAND
            return to
        else:
            return None

    def move_sand_step(self, pos: tuple[int, int]) -> tuple[int, int]:
        x, y = pos
        return (
            self._try_move(pos, (x, y + 1))
            or self._try_move(pos, (x - 1, y + 1))
            or self._try_move(pos, (x + 1, y + 1))
            or pos
        )

    def run_sand(self) -> tuple[int, int]:
        self.sand_count += 1
        oldpos = None
        pos = self.sand_gen
        while oldpos != pos and pos[1] < self.abyss:
            oldpos, pos = pos, self.move_sand_step(pos)
        return pos

    def run_sand_until_abyss(self) -> int:
        while self.run_sand()[1] < self.abyss:
            pass
        return self.sand_count

    def run_sand_until_full(self) -> int:
        while self.run_sand() != self.sand_gen:
            pass
        return self.sand_count

    def print_state(self) -> None:
        for y in range(self.range[1], self.range[3] + 2 + 1):
            s = f"{y: 4} "
            for x in range(
                min(t[0] for t in self.grid),
                max(t[0] for t in self.grid) + 1,
            ):
                if (x, y) == self.sand_gen:
                    s += "+"
                elif y == self.floor:
                    assert self.grid.get((x, y), Material.AIR) == Material.AIR
                    s += "-"
                else:
                    s += self.grid.get((x, y), Material.AIR).to_char()
            print(s)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()

    g = Grid(get_lines(PUZZLE_INPUT), (500, 0))
    print(f"Part 1: {g.run_sand_until_abyss()}")

    print(f"Part 2: {g.run_sand_until_full()}")
