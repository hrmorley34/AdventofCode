from __future__ import annotations

from collections import defaultdict
from puzzle_input import puzzle_input
from queue import PriorityQueue
import string
from typing import Any, Iterable, Mapping, NewType, Sequence, cast


DIRECTIONS = frozenset((1, 1j, -1, -1j))
SEARCH_DIRECTIONS = frozenset((-1, -1j))  # up and left only (already created nodes)


Key = NewType("Key", str)
KEYS = cast(Sequence[Key], string.ascii_lowercase)
DOORS = cast(Sequence[Key], string.ascii_uppercase)


class Node:
    connections: dict[Node, int]  # int distance

    def __init__(self) -> None:
        self.connections = dict()

    def __hash__(self) -> int:
        return id(self)

    def __repr__(self) -> str:
        return f"{type(self).__name__}({len(self.connections)} conn)"

    def new(self) -> Node:
        return type(self)()


class CommonKeyedNode(Node):
    key: Key

    def __init__(self, key: Key | str | None = None) -> None:
        super().__init__()

        if key is not None:
            self.key = Key(key.lower())

    def __repr__(self) -> str:
        return f"{type(self).__name__}({len(self.connections)} conn, key={self.key})"

    def new(self) -> Node:
        return type(self)(self.key)


class KeyNode(CommonKeyedNode):
    pass


class DoorNode(CommonKeyedNode):
    pass


def look_for_connections(
    nodes: Mapping[complex, Node], point: complex, directions: Iterable[int | complex]
):
    this = nodes[point]
    for direction in directions:
        search = point + direction
        distance = 1
        while search not in nodes:
            search += direction
            distance += 1
            if search.real < 0 or search.imag < 0:
                raise Exception
        found = nodes[search]
        this.connections[found] = found.connections[this] = distance


def construct_nodes(maze: Mapping[complex, str]):
    nodes: dict[complex, Node] = dict()
    entrance: Node | None = None
    keys: set[Key] = set()

    for key, value in maze.items():
        if value == "#":
            continue  # wall
        elif value == "@":
            nodes[key] = entrance = Node()
            created = True
        elif value in KEYS:
            nodes[key] = n = KeyNode(value)
            keys.add(n.key)
            created = True
        elif value in DOORS:
            nodes[key] = DoorNode(value)
            created = True
        else:  # value == "."
            created = False  # don't create a node yet

        neighbours = {d for d in DIRECTIONS if maze.get(key + d, "#") != "#"}
        neighbourcount = len(neighbours)

        if not created:  # no point checking if we need one if there is one
            necessary = False
            if neighbourcount == 0:  # shouldn't happen; isolated point can be ignored
                pass
            elif neighbourcount == 1:  # dead end
                necessary = True
            elif neighbourcount == 2:
                dirs = cast("tuple[complex | int, complex | int]", tuple(neighbours))
                necessary = dirs[0] != -dirs[1]  # corners but not straight corridors
            else:  # 3 or 4
                necessary = True

            if necessary:
                nodes[key] = Node()
                created = True

        if created:
            look_for_connections(nodes, key, neighbours & SEARCH_DIRECTIONS)

    assert entrance is not None
    return entrance, nodes, keys


def _find_direct(startnode: Node) -> dict[Node, int]:
    seen: set[Node] = set()
    to_do: PriorityQueue[tuple[int, int, Node]] = PriorityQueue()
    to_do.put((0, id(startnode), startnode))
    out: dict[Node, int] = {}
    while not to_do.empty():
        basedist, _, node = to_do.get()
        seen.add(node)
        for conn, dist in node.connections.items():
            # print(node, conn, dist, to_do.qsize())
            if conn is startnode:
                continue
            elif isinstance(conn, (KeyNode, DoorNode)):
                seen.add(conn)
                if conn not in out or out[conn] > basedist + dist:
                    out[conn] = basedist + dist
            elif conn not in seen:
                to_do.put((basedist + dist, id(conn), conn))
    return out


def increase_directness(entrance: Node, nodes: dict[complex, Node]):
    new_nodes: dict[complex, Node] = {}
    new_connections: defaultdict[Node, dict[complex, int]] = defaultdict(dict)
    invlookup = {nodes[k]: k for k in nodes}

    new_entrance = None
    for key, node in nodes.items():
        if not isinstance(node, (DoorNode, KeyNode)) and node is not entrance:
            continue

        new_node = node.new()
        if node is entrance:
            new_entrance = new_node

        new_nodes[key] = new_node
        new_connections[new_node] = {
            invlookup[k]: v for k, v in _find_direct(node).items()
        }

    assert new_entrance is not None
    for node, d in new_connections.items():
        node.connections = {new_nodes[pos]: dist for pos, dist in d.items()}

    return new_entrance, new_nodes


class Runner:
    current_node: Node
    visited_nodes: set[Node]
    keys: set[Key]
    distance: int
    route: str

    def __init__(self, start: Node) -> None:
        self.current_node = start
        self.visited_nodes = {start}
        self.keys = set()
        self.distance = 0
        self.route = "@"

    def __hash__(self) -> int:
        return id(self)

    def __repr__(self) -> str:
        return (
            f"{type(self).__name__}({self.distance}, {self.current_node} conn, "
            + ("".join(self.keys) if self.keys else "-")
            + ")"
        )

    def copy(self) -> Runner:
        r = type(self)(self.current_node)
        r.visited_nodes.update(self.visited_nodes)
        r.keys.update(self.keys)
        r.distance = self.distance
        r.route = self.route
        return r

    def go_to(self, node: Node, distance: int):
        if isinstance(node, DoorNode) and node.key not in self.keys:
            raise ValueError

        self.current_node = node
        self.distance += distance

        if isinstance(node, KeyNode):
            if node.key not in self.keys:
                self.keys.add(node.key)
                self.route += node.key
                self.visited_nodes.clear()  # reset visited nodes as, having got the key, one may need to go back on oneself

        self.visited_nodes.add(node)

    def flood(self):
        rs: set[Runner] = set()
        for conn, dist in self.current_node.connections.items():
            if conn in self.visited_nodes:
                continue
            if isinstance(conn, DoorNode) and conn.key not in self.keys:
                continue  # can't go there; no key for it

            r = self.copy()
            rs.add(r)
            r.go_to(conn, dist)
        return rs

    def _get_metric(self):
        return (self.distance, -len(self.keys))

    def __lt__(self, obj: Any):
        if isinstance(obj, Runner):
            return self._get_metric() < obj._get_metric()
        return NotImplemented

    def __le__(self, obj: Any):
        if isinstance(obj, Runner):
            return self._get_metric() <= obj._get_metric()
        return NotImplemented

    def __gt__(self, obj: Any):
        if isinstance(obj, Runner):
            return self._get_metric() > obj._get_metric()
        return NotImplemented

    def __ge__(self, obj: Any):
        if isinstance(obj, Runner):
            return self._get_metric() >= obj._get_metric()
        return NotImplemented


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().strip().splitlines(False)
    # PUZZLE_INPUT = clipboard.strip().splitlines(False)
    MAZE = {
        x + y * 1j: c
        for (y, line) in enumerate(PUZZLE_INPUT)
        for (x, c) in enumerate(line)
    }
    start, nodes, keys = construct_nodes(MAZE)
    start, nodes = increase_directness(start, nodes)

    rs: PriorityQueue[Runner] = PriorityQueue()
    rs_done: set[tuple[Node, frozenset[Key]]] = set()
    rs.put(Runner(start=start))
    best = None

    # iteration = 0
    # lastd = 0
    while not rs.empty():
        r = rs.get()

        if (r.current_node, frozenset(r.keys)) in rs_done:
            continue
        rs_done.add((r.current_node, frozenset(r.keys)))

        # print(r, invnodes[r.current_node], rs.qsize())
        # print({invnodes[n] for n in r.visited_nodes})

        if r.keys == keys:
            best = r
            # raise Exception
            break
        for i in r.flood():
            if (i.current_node, frozenset(i.keys)) in rs_done:
                continue

            # print(" ", i, invnodes[i.current_node])
            rs.put(i)

        # if r.distance > lastd:
        #     lastd = r.distance
        #     print(lastd, rs.qsize())

        # iteration += 1
        # if iteration % 1000_000 == 0:
        #     print(iteration)
        #     break
    print(best.distance)
