from enum import Enum
from typing import Any, Iterable

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

    def __neg__(self) -> "Facing":
        return Facing((self.value + 2) % 4)

    def __sub__(self, r: Any) -> "Rotate":
        if isinstance(r, Facing):
            return Rotate.from_unwrapped(self.value - r.value)
        return NotImplemented

    def as_arrow(self):
        return ">v<^"[self.value]


class Rotate(Enum):
    L = LEFT = -1
    R = RIGHT = 1
    ZERO = 0
    HALF = 2

    @classmethod
    def from_unwrapped(cls, i: int):
        return cls(((i + 1) % 4) - 1)


class Node:
    right: tuple["Node", Rotate]
    down: tuple["Node", Rotate]
    left: tuple["Node", Rotate]
    up: tuple["Node", Rotate]
    wall: bool
    position: tuple[int, int]
    text: str

    @property
    def directions(self):
        return self.right, self.down, self.left, self.up

    def get_direction(self, d: Facing) -> tuple["Node", Rotate]:
        return self.directions[d.value]

    def set_direction(self, d: Facing, v: tuple["Node", Rotate]) -> None:
        if d == Facing.RIGHT:
            self.right = v
        elif d == Facing.DOWN:
            self.down = v
        elif d == Facing.LEFT:
            self.left = v
        elif d == Facing.UP:
            self.up = v
        else:
            raise ValueError

    def __init__(self, pos: tuple[int, int], wall: bool) -> None:
        self.position = pos
        self.wall = wall
        self.text = "#" if wall else "."


def parse_grid(grid: str) -> tuple[Node, dict[tuple[int, int], Node]]:
    lines = grid.splitlines()
    first_node: Node | None = None
    first_of_x: dict[int, Node] = {}
    last_of_x: dict[int, Node] = {}
    first_of_y: Node | None = None
    last_of_y: Node | None = None
    nodes: dict[tuple[int, int], Node] = {}

    for y, row in enumerate(lines, 1):
        first_of_y = last_of_y = None
        for x, cell in enumerate(row, 1):
            if cell in ".#":
                n = Node((x, y), cell == "#")
                nodes[n.position] = n
                if first_node is None:
                    first_node = n
                if last_of_x.get(x):
                    n.up = last_of_x[x], Rotate.ZERO
                    last_of_x[x].down = n, Rotate.ZERO
                    last_of_x[x] = n
                else:
                    first_of_x[x] = last_of_x[x] = n
                if last_of_y:
                    n.left = last_of_y, Rotate.ZERO
                    last_of_y.right = n, Rotate.ZERO
                    last_of_y = n
                else:
                    first_of_y = last_of_y = n
        assert first_of_y and last_of_y
        first_of_y.left = last_of_y, Rotate.ZERO
        last_of_y.right = first_of_y, Rotate.ZERO
    for x in first_of_x:
        first_of_x[x].up = last_of_x[x], Rotate.ZERO
        last_of_x[x].down = first_of_x[x], Rotate.ZERO
    assert first_node
    return first_node, nodes


def parse_cubegrid(
    grid: str, panelsize: int
) -> tuple[Node, dict[tuple[int, int], Node]]:
    lines = grid.splitlines()
    max_line_len = max(map(len, lines))

    first_node: Node | None = None
    panel_seams: dict[
        tuple[int, int],
        dict[Facing, dict[int, Node]],
    ] = {}
    nodes: dict[tuple[int, int], Node] = {}

    for panel_y in range(len(lines) // panelsize):
        for panel_x in range(max_line_len // panelsize):
            if lines[panel_y * panelsize][
                panel_x * panelsize : panel_x * panelsize + 1
            ] not in (".", "#"):
                continue

            first_of_x: dict[int, Node] = {}
            last_of_x: dict[int, Node] = {}
            first_of_y: dict[int, Node] = {}
            last_of_y: dict[int, Node] = {}

            for dy, row in enumerate(
                lines[panel_y * panelsize : (panel_y + 1) * panelsize], 1
            ):
                y = panel_y * panelsize + dy
                for dx, cell in enumerate(
                    row[panel_x * panelsize : (panel_x + 1) * panelsize], 1
                ):
                    x = panel_x * panelsize + dx

                    if cell in ".#":
                        n = Node((x, y), cell == "#")
                        nodes[n.position] = n
                        if first_node is None:
                            first_node = n
                        if last_of_x.get(dx):
                            n.up = last_of_x[dx], Rotate.ZERO
                            last_of_x[dx].down = n, Rotate.ZERO
                            last_of_x[dx] = n
                        else:
                            first_of_x[dx] = last_of_x[dx] = n
                        if last_of_y.get(dy):
                            n.left = last_of_y[dy], Rotate.ZERO
                            last_of_y[dy].right = n, Rotate.ZERO
                            last_of_y[dy] = n
                        else:
                            first_of_y[dy] = last_of_y[dy] = n
            panel_seams[panel_x, panel_y] = {
                Facing.UP: first_of_x,
                Facing.DOWN: last_of_x,
                Facing.LEFT: first_of_y,
                Facing.RIGHT: last_of_y,
            }

    assert first_node

    assert len(panel_seams) == 6, panel_seams.keys()

    for panel_y in range(len(lines) // panelsize):
        for panel_x in range(max_line_len // panelsize):
            if (panel_x, panel_y) not in panel_seams:
                continue

            # Direct neighbour left-right
            if (panel_x + 1, panel_y) in panel_seams:
                left = panel_seams[panel_x, panel_y].pop(Facing.RIGHT)
                right = panel_seams[panel_x + 1, panel_y].pop(Facing.LEFT)
                for dy in range(1, panelsize + 1):
                    leftn, rightn = left[dy], right[dy]
                    leftn.right = rightn, Rotate.ZERO
                    rightn.left = leftn, Rotate.ZERO
            # Direct neighbour up-down
            if (panel_x, panel_y + 1) in panel_seams:
                top = panel_seams[panel_x, panel_y].pop(Facing.DOWN)
                bottom = panel_seams[panel_x, panel_y + 1].pop(Facing.UP)
                for dx in range(1, panelsize + 1):
                    topn, bottomn = top[dx], bottom[dx]
                    topn.down = bottomn, Rotate.ZERO
                    bottomn.up = topn, Rotate.ZERO

            for (dx, dy), requires, thisside, dside, rev in (
                # 1-1 diagonals
                ((-1, 1), {(0, 1)}, Facing.LEFT, Facing.UP, False),
                ((-1, 1), {(-1, 0)}, Facing.DOWN, Facing.RIGHT, False),
                ((1, 1), {(1, 0)}, Facing.DOWN, Facing.LEFT, True),
                ((1, 1), {(0, 1)}, Facing.RIGHT, Facing.UP, True),
                # 2-1 diagonals
                ((-2, 1), {(0, 1), (-1, 1)}, Facing.UP, Facing.UP, True),
                ((-2, 1), {(-1, 0), (-2, 0)}, Facing.DOWN, Facing.DOWN, True),
                ((-1, 2), {(-1, 0), (-1, 1)}, Facing.RIGHT, Facing.RIGHT, True),
                ((-1, 2), {(0, 1), (0, 2)}, Facing.LEFT, Facing.LEFT, True),
                ((1, 2), {(0, 1), (0, 2)}, Facing.RIGHT, Facing.RIGHT, True),
                ((1, 2), {(1, 0), (1, 1)}, Facing.LEFT, Facing.LEFT, True),
                ((2, 1), {(0, 1), (1, 1)}, Facing.UP, Facing.UP, True),
                ((2, 1), {(1, 0), (2, 0)}, Facing.DOWN, Facing.DOWN, True),
                # 3-0 - not required
                # ((3, 0), {(1, 0), (2, 0)}, Facing.LEFT, Facing.RIGHT, False),
                # ((0, 3), {(0, 1), (0, 2)}, Facing.UP, Facing.DOWN, False),
                # 3-1 (''|. - 2-1-1)
                (
                    (3, 1),  # .|''<
                    {(1, 0), (2, 0), (2, 1)},
                    Facing.LEFT,
                    Facing.DOWN,
                    True,
                ),
                (
                    (-3, 1),  # >''|.
                    {(-1, 0), (-2, 0), (-2, 1)},
                    Facing.RIGHT,
                    Facing.DOWN,
                    True,
                ),
                (
                    (-1, 3),
                    {(0, 1), (0, 2), (-1, 2)},
                    Facing.UP,
                    Facing.LEFT,
                    False,
                ),
                (
                    (1, 3),
                    {(0, 1), (0, 2), (1, 2)},
                    Facing.UP,
                    Facing.RIGHT,
                    False,
                ),
                (
                    (-3, 1),  # ..|'<
                    {(-1, 0), (-1, 1), (-2, 1)},
                    Facing.UP,
                    Facing.LEFT,
                    False,
                ),
                (
                    (3, 1),  # >'|..
                    {(1, 0), (1, 1), (2, 1)},
                    Facing.UP,
                    Facing.RIGHT,
                    True,
                ),
                (
                    (-1, 3),
                    {(0, 1), (-1, 1), (-1, 2)},
                    Facing.RIGHT,
                    Facing.DOWN,
                    False,
                ),
                (
                    (1, 3),
                    {(0, 1), (1, 1), (1, 2)},
                    Facing.LEFT,
                    Facing.DOWN,
                    True,
                ),
                # 3-2 (1-2-1-1)
                (
                    (-2, 3),
                    {(-1, 0), (-1, 1), (-1, 2), (-2, 2)},
                    Facing.UP,
                    Facing.DOWN,
                    False,
                ),
                # can't be bothered to do any more
            ):
                if any(
                    (panel_x + rdx, panel_y + rdy) not in panel_seams
                    for rdx, rdy in requires
                ):
                    continue
                thisrot = (-dside) - thisside
                drot = (-thisside) - dside
                if (panel_x + dx, panel_y + dy) in panel_seams:
                    ts = panel_seams[panel_x, panel_y].pop(thisside)
                    ds = panel_seams[panel_x + dx, panel_y + dy].pop(dside)
                    for dv in range(1, panelsize + 1):
                        tn, dn = ts[dv], ds[panelsize + 1 - dv if rev else dv]
                        tn.set_direction(thisside, (dn, thisrot))
                        dn.set_direction(dside, (tn, drot))

    assert all(len(seams) == 0 for seams in panel_seams.values()), {
        d: len(seams) for d, seams in panel_seams.items()
    }

    return first_node, nodes


def print_grid(nodes: dict[tuple[int, int], Node]):
    min_x = min(pos[0] for pos in nodes)
    max_x = max(pos[0] for pos in nodes)
    min_y = min(pos[1] for pos in nodes)
    max_y = max(pos[1] for pos in nodes)
    for y in range(min_y, max_y + 1):
        s = ""
        for x in range(min_x, max_x + 1):
            n = nodes.get((x, y))
            s += n.text if n else " "
        print(s)


def parse_directions(s: str):
    n = ""
    for c in s + "E":
        if c not in "LRE":
            n += c
        else:
            yield int(n)
            n = ""
            if c == "L":
                yield Rotate.L
            elif c == "R":
                yield Rotate.R


def follow_move(node: Node, count: int, direction: Facing) -> tuple[Node, Facing]:
    for _ in range(count):
        node.text = direction.as_arrow()
        next, r = node.get_direction(direction)
        if next.wall:
            return node, direction
        node = next
        direction += r
    return node, direction


def traverse_map(start_node: Node, moves: Iterable[int | Rotate]) -> int:
    node = start_node
    direction = Facing.RIGHT
    for m in moves:
        if isinstance(m, Rotate):
            direction += m
        else:
            node, direction = follow_move(node, m, direction)
    x, y = node.position
    return 1000 * y + 4 * x + direction.value


if __name__ == "__main__":
    MAP, PATH = puzzle_input().split("\n\n")

    MOVES = list(parse_directions(PATH))

    START_NODE, NODES = parse_grid(MAP)
    print(f"Part 1: {traverse_map(START_NODE, MOVES)}")
    # print_grid(NODES)

    # PANEL_SIZE = 4
    PANEL_SIZE = 50

    START_NODE2, NODES2 = parse_cubegrid(MAP, PANEL_SIZE)
    print(f"Part 2: {traverse_map(START_NODE2, MOVES)}")
