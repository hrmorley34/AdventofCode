from collections import defaultdict
from typing import Any, NamedTuple

from puzzle_input import puzzle_input


class Position(NamedTuple):
    x: int
    y: int

    def __add__(self, pos: Any) -> "Position":
        if isinstance(pos, Position):
            return Position(self.x + pos.x, self.y + pos.y)
        return NotImplemented


N = Position(0, -1)
NE = Position(1, -1)
E = Position(1, 0)
SE = Position(1, 1)
S = Position(0, 1)
SW = Position(-1, 1)
W = Position(-1, 0)
NW = Position(-1, -1)
NORTHY = {N, NE, NW}
EASTY = {E, NE, SE}
SOUTHY = {S, SE, SW}
WESTY = {W, NW, SW}
ADJ = {N, NE, E, SE, S, SW, W, NW}
DIRGROUPS = {N: NORTHY, E: EASTY, S: SOUTHY, W: WESTY}
DIRGROUPS_ORDER = [N, S, W, E]


class Grid:
    grid: set[Position]
    movecount: int

    @property
    def dirgroups(self) -> list[Position]:
        m = self.movecount % 4
        return DIRGROUPS_ORDER[m:] + DIRGROUPS_ORDER[:m]

    def get_pos(self, pos: Position) -> bool:
        return pos in self.grid

    def get_range(self) -> tuple[Position, Position]:
        return (
            Position(min(p.x for p in self.grid), min(p.y for p in self.grid)),
            Position(max(p.x for p in self.grid), max(p.y for p in self.grid)),
        )

    def get_empty_ground_tiles(self):
        minp, maxp = self.get_range()
        occupied = len(
            {
                e
                for e in self.grid
                if minp.x <= e.x <= maxp.x and minp.y <= e.y <= maxp.y
            }
        )
        total = (maxp.x - minp.x + 1) * (maxp.y - minp.y + 1)
        return total - occupied

    def consider_move(self, pos: Position) -> Position:
        adj = {p: self.get_pos(pos + p) for p in ADJ}
        if not any(adj.values()):
            return pos
        for dir in self.dirgroups:
            group = DIRGROUPS[dir]
            if not any(adj[p] for p in group):
                return pos + dir
        return pos

    def move_elves(self) -> bool:
        proposed_positions: defaultdict[Position, set[Position]] = defaultdict(set)
        for pos in self.grid:
            d = self.consider_move(pos)
            if d != pos:
                proposed_positions[d].add(pos)
        any_moves = False
        for newpos, oldpositions in proposed_positions.items():
            if len(oldpositions) == 1:
                self.grid.remove(oldpositions.pop())
                self.grid.add(newpos)
                any_moves = True
        self.movecount += 1
        return any_moves

    def __init__(self) -> None:
        self.grid = set()
        self.movecount = 0

    def parse_lines(self, s: str) -> None:
        for y, line in enumerate(s.splitlines()):
            for x, char in enumerate(line):
                if char == "#":
                    self.grid.add(Position(x, y))

    def print(self) -> None:
        minp, maxp = self.get_range()
        for y in range(minp.y, maxp.y + 1):
            line = ""
            for x in range(minp.x, maxp.x + 1):
                line += "#" if self.get_pos(Position(x, y)) else "."
            print(line)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()

    g = Grid()
    g.parse_lines(PUZZLE_INPUT)
    for _ in range(10):
        g.move_elves()
        # print(f"=== {g.movecount} ===")
        # g.print()
    i = g.get_empty_ground_tiles()
    print(f"Part 1: {i}")

    while g.move_elves():
        if g.movecount % 1_000 == 0:
            print(g.movecount)
    print(f"Part 2: {g.movecount}")
