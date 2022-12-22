from enum import Enum
from typing import Any

from puzzle_input import puzzle_input


class Facing(Enum):
    RIGHT = 0
    DOWN = 1
    LEFT = 2
    UP = 3

    def __add__(self, r: Any) -> "Facing":
        if isinstance(r, Rotate):
            return Facing((self.value + r.value) % 4)
        return NotImplemented

    def as_arrow(self):
        return ">v<^"[self.value]


class Rotate(Enum):
    R = 1
    L = -1


class Node:
    right: "Node"
    down: "Node"
    left: "Node"
    up: "Node"
    wall: bool
    position: tuple[int, int]
    text: str

    @property
    def directions(self):
        return self.right, self.down, self.left, self.up

    def get_direction(self, d: Facing) -> "Node":
        return self.directions[d.value]

    def __init__(self, pos: tuple[int, int], wall: bool) -> None:
        self.position = pos
        self.wall = wall
        self.text = "#" if wall else "."


def parse_grid(grid: str) -> list[Node]:
    lines = grid.splitlines()
    nodes: list[Node] = []
    first_of_x: dict[int, Node] = {}
    last_of_x: dict[int, Node] = {}
    first_of_y: Node | None = None
    last_of_y: Node | None = None

    for y, row in enumerate(lines, 1):
        first_of_y = last_of_y = None
        for x, cell in enumerate(row, 1):
            if cell in ".#":
                n = Node((x, y), cell == "#")
                nodes.append(n)
                if last_of_x.get(x):
                    n.up = last_of_x[x]
                    last_of_x[x].down = n
                    last_of_x[x] = n
                else:
                    first_of_x[x] = last_of_x[x] = n
                if last_of_y:
                    n.left = last_of_y
                    last_of_y.right = n
                    last_of_y = n
                else:
                    first_of_y = last_of_y = n
        assert first_of_y and last_of_y
        first_of_y.left = last_of_y
        last_of_y.right = first_of_y
    for x in first_of_x:
        first_of_x[x].up = last_of_x[x]
        last_of_x[x].down = first_of_x[x]
    return nodes


def print_grid(nodes: list[Node]):
    noded = {n.position: n for n in nodes}
    min_x = min(pos[0] for pos in noded)
    max_x = max(pos[0] for pos in noded)
    min_y = min(pos[1] for pos in noded)
    max_y = max(pos[1] for pos in noded)
    for y in range(min_y, max_y + 1):
        s = ""
        for x in range(min_x, max_x + 1):
            n = noded.get((x, y))
            s += n.text if n else " "
        print(s)


def parse_directions(s: str):
    n = ""
    direction = Facing.RIGHT
    for c in s + "E":
        if c not in "LRE":
            n += c
        else:
            yield int(n), direction
            n = ""
            if c == "L":
                direction += Rotate.L
            elif c == "R":
                direction += Rotate.R


def follow_move(node: Node, count: int, direction: Facing) -> Node:
    for _ in range(count):
        node.text = direction.as_arrow()
        next = node.get_direction(direction)
        if next.wall:
            return node
        node = next
    return node


if __name__ == "__main__":
    MAP, PATH = puzzle_input().split("\n\n")

    NODES = parse_grid(MAP)
    START_NODE = NODES[0]

    MOVES = list(parse_directions(PATH))

    node = START_NODE
    direction = Facing.RIGHT
    for count, direction in MOVES:
        node = follow_move(node, count, direction)
    # print_grid(NODES)
    x, y = node.position
    n = 1000 * y + 4 * x + direction.value
    print(f"Part 1: {n}")
