from __future__ import annotations

from dataclasses import dataclass
from queue import PriorityQueue
from puzzle_input import puzzle_input
from typing import Any, Iterable


@dataclass(init=True, eq=True, frozen=True)
class Cell:
    x: int
    y: int
    risk: int


class Runner:
    cell: Cell
    risk: int

    def __init__(self, cell: Cell, risk: int) -> None:
        self.cell = cell
        self.risk = risk

    def copy(self) -> Runner:
        return type(self)(self.cell, self.risk)

    def move(self, cell: Cell):
        self.cell = cell
        self.risk += cell.risk

    def __lt__(self, obj: Any) -> bool:
        if isinstance(obj, Runner):
            return self.risk < obj.risk
        return NotImplemented

    def __gt__(self, obj: Any) -> bool:
        if isinstance(obj, Runner):
            return self.risk > obj.risk
        return NotImplemented


class Map:
    _map: dict[tuple[int, int], Cell]

    def __init__(self, pinput: Iterable[Iterable[int]]) -> None:
        self._map = {
            (x, y): Cell(x, y, risk)
            for y, line in enumerate(pinput)
            for x, risk in enumerate(line)
        }

    def __getitem__(self, item: tuple[int, int]) -> Cell:
        return self._map[item]

    def __contains__(self, item: tuple[int, int] | Cell) -> bool:
        if isinstance(item, Cell):
            return item in self._map.values()
        else:
            return item in self._map

    def neighbours(self, coords: tuple[int, int] | Cell) -> Iterable[Cell]:
        if isinstance(coords, Cell):
            coords = (coords.x, coords.y)
        for pair in (
            (coords[0] + 1, coords[1]),
            (coords[0], coords[1] + 1),
            (coords[0] - 1, coords[1]),
            (coords[0], coords[1] - 1),
        ):
            if pair in self._map:
                yield self._map[pair]


def create_map2(pinput: Iterable[Iterable[int]]) -> Map:
    return Map(
        ((i + xv + yv - 1) % 9 + 1 for xv in range(5) for i in row)
        for yv in range(5)
        for row in pinput
    )


def dijkstra_again(map_: Map, start: Cell, end: Cell) -> Runner:
    queue: PriorityQueue[Runner] = PriorityQueue()
    seenpositions: set[Cell] = set()
    queue.put(Runner(map_[0, 0], 0))

    while True:
        r = queue.get()
        if r.cell == end:
            return r

        if r.cell in seenpositions:
            continue
        seenpositions.add(r.cell)

        for cell in map_.neighbours(r.cell):
            r2 = r.copy()
            r2.move(cell)
            queue.put(r2)


if __name__ == "__main__":
    PUZZLE_INPUT = [[int(i) for i in line] for line in puzzle_input().splitlines()]

    MAP = Map(PUZZLE_INPUT)
    print(
        "Part 1:",
        dijkstra_again(
            MAP, MAP[0, 0], MAP[len(PUZZLE_INPUT[-1]) - 1, len(PUZZLE_INPUT) - 1]
        ).risk,
    )

    MAP2 = create_map2(PUZZLE_INPUT)
    print(
        "Part 2:",
        dijkstra_again(
            MAP2,
            MAP2[0, 0],
            MAP2[len(PUZZLE_INPUT[-1]) * 5 - 1, len(PUZZLE_INPUT) * 5 - 1],
        ).risk,
    )
