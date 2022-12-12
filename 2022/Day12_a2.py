import string
from dataclasses import dataclass, field
from math import inf, sqrt
from heapq import heappush, heappop
from typing import Any, Callable

from puzzle_input import puzzle_input

ELEVATION_MAP = string.ascii_lowercase
ELEVATION_MAP_2 = dict(S=0, E=25)


def to_elevation(s: str) -> int:
    try:
        return ELEVATION_MAP.index(s)
    except ValueError:
        return ELEVATION_MAP_2[s]


@dataclass()
class Node:
    x: int
    y: int
    elevation: int
    neighbours: list["Node"] = field(default_factory=list, repr=False)
    reverse_neighbours: list["Node"] = field(default_factory=list, repr=False)


@dataclass()
class Token:
    node: Node
    distance_travelled: tuple[int, "Token | None"] | None  # dijkstra distance
    distance_to_go: float  # A* distance heuristic - euclidean distance
    elevation_target: int
    neighbours: list["Token"] = field(default_factory=list, repr=False)

    @property
    def sort_key(self):
        return (
            (inf if self.distance_travelled is None else self.distance_travelled[0])
            + self.distance_to_go
            + self.elevation_target
        )

    @property
    def pos(self):
        return self.node.x, self.node.y

    def __init__(self, node: Node, target: Node | None) -> None:
        self.node = node
        self.distance_travelled = None
        if target is None:
            # aim for left - all bs are in column 1 so must go there
            self.distance_to_go = node.x > 2 and node.x
            self.elevation_target = self.node.elevation
        else:
            self.distance_to_go = sqrt(
                (node.x - target.x) ** 2 + (node.y - target.y) ** 2
            )
            self.elevation_target = 25 - self.node.elevation
        self.neighbours = []

    def __lt__(self, o: Any) -> bool:
        if isinstance(o, Token):
            return self.sort_key < o.sort_key
        return NotImplemented


def print_path(max_x: int, max_y: int, path: list[Token]) -> None:
    path_pos = {t.pos for t in path}
    for y in range(max_y):
        s = ""
        for x in range(max_x):
            s += "#" if (x, y) in path_pos else "."
        print(s)


def astar(
    start_token: Token, end_condition: Callable[[Token], bool]
) -> tuple[int, list[Token]]:
    queue: list[Token] = [start_token]
    seen: set[tuple[int, int]] = set()
    while True:
        t = heappop(queue)
        if end_condition(t):
            break
        if t.pos in seen:
            continue

        assert t.distance_travelled is not None
        for n in t.neighbours:
            if n.pos in seen:
                continue
            if (
                n.distance_travelled is None
                or n.distance_travelled[0] > t.distance_travelled[0] + 1
            ):
                n.distance_travelled = (t.distance_travelled[0] + 1, t)
                heappush(queue, n)
        seen.add(t.pos)

    END = t
    assert END.distance_travelled is not None

    path = [END]
    while (
        path[0].distance_travelled is not None
        and path[0].distance_travelled[1] is not None
    ):
        path.insert(0, path[0].distance_travelled[1])

    return END.distance_travelled[0], path


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    # SIZE = (len(PUZZLE_INPUT[0]), len(PUZZLE_INPUT))
    NODE_MAP: list[list[Node]] = []
    start = end = None
    for y, row in enumerate(PUZZLE_INPUT):
        NODE_MAP.append([])
        for x, c in enumerate(row):
            n = Node(x, y, to_elevation(c))
            NODE_MAP[-1].append(n)
            if c == "S":
                start = n
            elif c == "E":
                end = n
    assert start is not None
    assert end is not None

    for row in NODE_MAP:
        for node in row:
            neighbours = (
                NODE_MAP[node.y + dy][node.x + dx]
                for dx, dy in ((0, 1), (1, 0), (0, -1), (-1, 0))
                if 0 <= node.x + dx < len(row) and 0 <= node.y + dy < len(NODE_MAP)
            )
            filtered_neighbours = (
                n for n in neighbours if n.elevation - node.elevation <= 1
            )
            node.neighbours.extend(filtered_neighbours)
            for n in node.neighbours:
                n.reverse_neighbours.append(node)

    TOKENS: dict[tuple[int, int], Token] = {
        (x, y): Token(n, end)
        for y, row in enumerate(NODE_MAP)
        for x, n in enumerate(row)
    }
    TOKENS[start.x, start.y].distance_travelled = (0, None)
    for t in TOKENS.values():
        t.neighbours.extend(TOKENS[n.x, n.y] for n in t.node.neighbours)

    d, p = astar(TOKENS[start.x, start.y], lambda t: t.distance_to_go == 0)
    # print_path(*SIZE, p)
    print(f"Part 1: {d}")

    REVERSE_TOKENS: dict[tuple[int, int], Token] = {
        (x, y): Token(n, None)
        for y, row in enumerate(NODE_MAP)
        for x, n in enumerate(row)
    }
    REVERSE_TOKENS[end.x, end.y].distance_travelled = (0, None)
    for t in REVERSE_TOKENS.values():
        t.neighbours.extend(REVERSE_TOKENS[n.x, n.y] for n in t.node.reverse_neighbours)

    d, p = astar(REVERSE_TOKENS[end.x, end.y], lambda t: t.node.elevation == 0)
    # print_path(*SIZE, p)
    print(f"Part 2: {d}")
