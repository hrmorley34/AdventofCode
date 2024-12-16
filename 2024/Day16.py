from collections import defaultdict
from queue import PriorityQueue
from typing import Self

from puzzle_input import puzzle_input

PosDir = tuple[int, int, int]
Nodes = dict[PosDir, dict[PosDir, int]]
DIRECTIONS = [(1, 0), (0, 1), (-1, 0), (0, -1)]
EAST = 0
SCORE_MOVE = 1
SCORE_ROTATE = 1000


class Maze:
    nodes: Nodes
    start: PosDir
    end: tuple[int, int]

    def __init__(self, nodes: Nodes, start: PosDir, end: tuple[int, int]) -> None:
        self.nodes = nodes
        self.start = start
        self.end = end

    @classmethod
    def from_lines(cls, lines: list[str]) -> Self:
        nodes: defaultdict[PosDir, dict[PosDir, int]] = defaultdict(dict)
        start: PosDir = (-1, -1, EAST)
        end: tuple[int, int] = (-1, -1)
        for y, line in enumerate(lines[1:-1], 1):
            for x, c in enumerate(line[1:-1], 1):
                if c == "#":
                    continue
                if c == "S":
                    assert start == (-1, -1, EAST)
                    start = (x, y, EAST)
                elif c == "E":
                    assert end == (-1, -1)
                    end = (x, y)
                for i in range(len(DIRECTIONS)):
                    nodes[x, y, i][x, y, (i - 1) % len(DIRECTIONS)] = SCORE_ROTATE
                    nodes[x, y, i][x, y, (i + 1) % len(DIRECTIONS)] = SCORE_ROTATE
                for i, (dx, dy) in enumerate(DIRECTIONS):
                    if lines[y + dy][x + dx] != "#":
                        nodes[x, y, i][x + dx, y + dy, i] = SCORE_MOVE
        return cls(nodes, start, end)

    def explore(self) -> int:
        # Dijkstra
        seen_nodes: set[PosDir] = set()
        q: PriorityQueue[tuple[int, PosDir]] = PriorityQueue()
        q.put((0, self.start))
        while q.qsize():
            w, pd = q.get()
            if pd[:2] == self.end:
                return w
            if pd in seen_nodes:
                continue
            else:
                seen_nodes.add(pd)
            for neighbour, weight in self.nodes[pd].items():
                if neighbour not in seen_nodes:
                    q.put((w + weight, neighbour))
        raise ValueError


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    MAZE = Maze.from_lines(PUZZLE_INPUT)
    print("Part 1:", MAZE.explore())
