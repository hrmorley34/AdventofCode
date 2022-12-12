import string
from dataclasses import dataclass, field
from math import inf, sqrt
from typing import Callable

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
    distance_travelled: tuple[int, tuple[int, int] | None] | None  # dijkstra distance
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

    def __init__(self, node: Node, target: Node | None) -> None:
        self.node = node
        self.distance_travelled = None
        if target is None:
            self.distance_to_go = 0  # don't use heuristic
            self.elevation_target = self.node.elevation
        else:
            self.distance_to_go = sqrt(
                (node.x - target.x) ** 2 + (node.y - target.y) ** 2
            )
            self.elevation_target = 25 - self.node.elevation
        self.neighbours = []


def print_unseen(max_x: int, max_y: int, unseen: list[Token]) -> None:
    unseen_pos = {(t.node.x, t.node.y) for t in unseen}
    for y in range(max_y):
        s = ""
        for x in range(max_x):
            s += "!" if (x, y) in unseen_pos else "."
        print(s)


def print_path(max_x: int, max_y: int, path: list[Token]) -> None:
    path_pos = {(t.node.x, t.node.y) for t in path}
    for y in range(max_y):
        s = ""
        for x in range(max_x):
            s += "#" if (x, y) in path_pos else "."
        print(s)


def astar(
    tokens: dict[tuple[int, int], Token], condition: Callable[[Token], bool]
) -> tuple[int, list[Token]]:
    queue: list[Token] = list(tokens.values())
    queue.sort(key=lambda t: t.sort_key)
    t = queue.pop(0)
    while condition(t):
        # if t.distance_travelled is None:
        #     print_unseen(*max(tokens, key=lambda t: t[0] + t[1]), queue)
        # print((t.node.x, t.node.y), t.node.elevation, t.distance_travelled)
        assert t.distance_travelled is not None
        for n in t.neighbours:
            if (
                n.distance_travelled is None
                or n.distance_travelled[0] > t.distance_travelled[0] + 1
            ):
                n.distance_travelled = (
                    t.distance_travelled[0] + 1,
                    (t.node.x, t.node.y),
                )

        queue.sort(key=lambda t: t.sort_key)
        t = queue.pop(0)

    END = t
    assert END.distance_travelled is not None

    path = [END]
    while (
        path[0].distance_travelled is not None
        and path[0].distance_travelled[1] is not None
    ):
        path.insert(0, tokens[path[0].distance_travelled[1]])

    return END.distance_travelled[0], path


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
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

    d, p = astar(TOKENS, lambda t: t.distance_to_go > 0)
    # print([(t.node.x, t.node.y, t.node.elevation) for t in p])
    # print_path(*max(TOKENS, key=lambda t: t[0] + t[1]), p)
    print(f"Part 1: {d}")

    REVERSE_TOKENS: dict[tuple[int, int], Token] = {
        (x, y): Token(n, None)
        for y, row in enumerate(NODE_MAP)
        for x, n in enumerate(row)
    }
    REVERSE_TOKENS[end.x, end.y].distance_travelled = (0, None)
    for t in REVERSE_TOKENS.values():
        t.neighbours.extend(REVERSE_TOKENS[n.x, n.y] for n in t.node.reverse_neighbours)

    d, p = astar(REVERSE_TOKENS, lambda t: t.node.elevation != 0)
    # print([(t.node.x, t.node.y, t.node.elevation) for t in p])
    # print_path(*max(TOKENS, key=lambda t: t[0] + t[1]), p)
    print(f"Part 2: {d}")
