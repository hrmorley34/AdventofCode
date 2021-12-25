from __future__ import annotations

from enum import Enum
from puzzle_input import puzzle_input
from typing import NamedTuple


class Cucumber(Enum):
    none = "."
    east = ">"
    south = "v"


class Coord(NamedTuple):
    x: int
    y: int


GridT = list[list[Cucumber]]


def can_move(grid: GridT, from_: Coord) -> Coord | None:
    old = grid[from_.y][from_.x]
    if old == Cucumber.none:
        return None
    elif old == Cucumber.east:
        new_coord = Coord((from_.x + 1) % len(grid[from_.y]), from_.y)
    elif old == Cucumber.south:
        new_coord = Coord(from_.x, (from_.y + 1) % len(grid))
    else:
        raise Exception
    if grid[new_coord.y][new_coord.x] == Cucumber.none:
        return new_coord
    return None


def load_grid(s: str) -> GridT:
    grid: GridT = list()
    for line in s.splitlines():
        gridline: list[Cucumber] = list()
        for cell in line:
            gridline.append(Cucumber(cell))
        grid.append(gridline)
    return grid


def do_move(grid: GridT, type: Cucumber | None = None) -> bool:
    if type is None:
        # `|` forces both sides to be executed (unlike `or`)
        return do_move(grid, Cucumber.east) | do_move(grid, Cucumber.south)

    moves: set[tuple[Coord, Coord]] = set()
    for y in range(len(grid)):
        for x in range(len(grid[y])):
            if grid[y][x] == type:
                m = can_move(grid, Coord(x, y))
                if m is not None:
                    moves.add((Coord(x, y), m))
    for movefrom, moveto in moves:
        grid[moveto.y][moveto.x] = grid[movefrom.y][movefrom.x]
        grid[movefrom.y][movefrom.x] = Cucumber.none
    return bool(moves)


def render_grid(grid: GridT) -> str:
    return "\n".join("".join(c.value for c in line) for line in grid)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    grid = load_grid(PUZZLE_INPUT)
    i = 1
    while do_move(grid):
        # print(i)
        # print(render_grid(grid))
        # input()
        i += 1
    print("Part 1:", i)
