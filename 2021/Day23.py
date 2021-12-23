from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from puzzle_input import puzzle_input
from queue import PriorityQueue
from typing import Any, Generator


@dataclass
class Node:
    neighbours: set[NodeJoin]
    x: int
    y: int

    def __hash__(self) -> int:
        return hash((self.x, self.y))

    def add_join(self, distance: int, to: Node, requires: set[Node] | None = None):
        if requires is None:
            requires = set()
        j = NodeJoin(distance=distance, from_=self, to=to, requires=requires)
        self.neighbours.add(j)
        return j

    def __repr__(self) -> str:
        return (
            f"Node{self.x},{self.y}(neighbours={{"
            + ", ".join(f"{j.to.x},{j.to.y}" for j in self.neighbours)
            + "})"
        )


@dataclass
class Column(Node):
    maxheight: int
    letter: Amphipod
    stack: list[Column]

    def __hash__(self) -> int:
        return hash((self.x, self.y))

    def __repr__(self) -> str:
        return (
            f"Column{self.x},{self.y}(maxheight={self.maxheight}, letter={self.letter}, neighbours={{"
            + ", ".join(f"{j.to.x},{j.to.y}" for j in self.neighbours)
            + "})"
        )


@dataclass
class Corridor(Node):
    def __hash__(self) -> int:
        return hash((self.x, self.y))

    def __repr__(self) -> str:
        return (
            f"Corridor{self.x},{self.y}(neighbours={{"
            + ", ".join(f"{j.to.x},{j.to.y}" for j in self.neighbours)
            + "})"
        )


@dataclass
class NodeJoin:
    distance: int
    from_: Node
    to: Node
    requires: set[Node]

    def __hash__(self) -> int:
        return hash((self.from_, self.to))


class Amphipod(Enum):
    A = "A"
    B = "B"
    C = "C"
    D = "D"


AMPHIPOD_VALUES: dict[Amphipod, int] = {
    Amphipod.A: 1,
    Amphipod.B: 10,
    Amphipod.C: 100,
    Amphipod.D: 1000,
}


class RunnerConfig:
    sort_with_lockcount: bool = True


class Runner:
    positions: dict[Node, Amphipod]
    state: frozenset[tuple[Node, Amphipod]]
    distance: int
    movecount: int
    locked: frozenset[Node]
    config: RunnerConfig
    history: list[Runner]

    def __init__(self, positions: dict[Node, Amphipod], config: RunnerConfig) -> None:
        self.positions = positions
        self.state = self.positions_to_state(positions)
        self.distance = 0
        self.movecount = 0
        self.locked = frozenset()
        self.config = config

    @staticmethod
    def positions_to_state(
        positions: dict[Node, Amphipod]
    ) -> frozenset[tuple[Node, Amphipod]]:
        return frozenset(positions.items())

    def calculate_locked(self, final: frozenset[tuple[Node, Amphipod]]):
        locked: set[Node] = set(self.locked)
        state = {(n, am) for n, am in self.state if isinstance(n, Column)}
        for am in Amphipod:
            sortedl = {n.y: n for n, tam in state if tam == am}
            if not sortedl:
                continue
            for y in range(next(iter(sortedl.values())).maxheight, 0, -1):
                if y not in sortedl:
                    break
                n = sortedl[y]
                if n.letter != am:
                    break
                locked.add(n)
        self.locked = frozenset(locked)

    def node_occupied(self, node: Node):
        return node in self.positions

    def get_next_moves(
        self, final: frozenset[tuple[Node, Amphipod]]
    ) -> Generator[Runner, None, None]:
        for n, am in self.positions.items():
            # lock correct ones in place (don't try to move)
            if n in self.locked:
                continue

            for join in n.neighbours:
                if self.node_occupied(join.to):
                    continue
                if any(map(self.node_occupied, join.requires)):
                    continue
                if isinstance(join.to, Column):
                    if (join.to, am) not in final:
                        continue
                    if not all(
                        c in self.locked
                        for c in join.to.stack[join.to.stack.index(join.to) + 1 :]
                    ):
                        continue
                r = object.__new__(type(self))
                r.positions = self.positions.copy()
                r.positions.pop(n)
                r.positions[join.to] = am
                r.state = self.positions_to_state(r.positions)
                r.distance = self.distance + AMPHIPOD_VALUES[am] * join.distance
                r.movecount = self.movecount + join.distance
                r.locked = frozenset()
                r.config = self.config
                yield r

    def __lt__(self, obj: Any) -> bool:
        if isinstance(obj, Runner):
            if self.config.sort_with_lockcount:
                return (-len(self.locked), self.distance) < (
                    -len(obj.locked),
                    obj.distance,
                )
            return self.distance < obj.distance
        return NotImplemented


def repr_runner_positions(pos: dict[Node, Amphipod]) -> str:
    pos_by_xy = {(n.x, n.y): a for n, a in pos.items()}
    s = ""
    for y in range(5):
        for x in range(11):
            if (x, y) in pos_by_xy:
                s += pos_by_xy[(x, y)].value
            elif y == 0:
                s += "."
            elif x in {2, 4, 6, 8}:
                s += "."
            else:
                s += " "
        s += "\n"
    return s


def create_node_map(
    m: list[str],
) -> tuple[dict[Node, Amphipod], dict[Node, Amphipod]]:
    assert m[:2] == ["#############", "#...........#"]

    TOWERS = [2, 4, 6, 8]
    topnodes = {x: Corridor(set(), x, 0) for x in range(11) if x not in TOWERS}

    positions: dict[Node, Amphipod] = dict()
    destpositions: dict[Node, Amphipod] = dict()

    lines = [line[3:10:2] for line in m[2:-1]]
    for pos, dest, *nx in zip(
        TOWERS, (Amphipod.A, Amphipod.B, Amphipod.C, Amphipod.D), *lines
    ):
        nx: list[str]
        nodes: list[Column] = []
        nodes.extend(
            Column(set(), pos, y, len(nx), dest, nodes) for y in range(1, len(nx) + 1)
        )
        nodes_n: list[Node] = list(nodes)
        for x, topnode in topnodes.items():
            a = nodes[0].add_join(abs(pos - x) + 1, topnode)
            for i in range(min(x + 1, pos), max(x - 1, pos) + 1):
                if i in topnodes:
                    a.requires.add(topnodes[i])
            topnode.add_join(a.distance, nodes[0], a.requires.copy())
            for i, node in enumerate(nodes[1:], 1):
                node.add_join(a.distance + i, topnode, a.requires | set(nodes_n[:i]))
                topnode.add_join(a.distance + i, node, a.requires | set(nodes_n[:i]))

        for node, n in zip(nodes, nx):
            positions[node] = Amphipod(n)
            destpositions[node] = dest

    return positions, destpositions


def dijkstra_III(
    positions: dict[Node, Amphipod], final: dict[Node, Amphipod]
) -> Runner:
    conf = RunnerConfig()

    finalstate = Runner.positions_to_state(final)
    r0 = Runner(positions.copy(), conf)
    r0.calculate_locked(finalstate)
    r0.history = []

    queue: PriorityQueue[Runner] = PriorityQueue()
    queue.put(r0)
    seenpositions: set[frozenset[tuple[Node, Amphipod]]] = set()

    max_distance: int | None = None
    count_normal = 0
    count_seen = 0
    count_gt_max = 0
    while True:
        r = queue.get(False)
        if r.state == finalstate:
            if conf.sort_with_lockcount:
                max_distance = r.distance
                conf.sort_with_lockcount = False
                queue = PriorityQueue()
                queue.put(r0)
                seenpositions.clear()
                print(f"set max {max_distance}")
                continue
            else:
                return r

        if r.state in seenpositions:
            count_seen += 1
            continue
        count_normal += 1
        if count_normal % 10 ** 4 == 0:
            print(
                count_normal,
                count_seen,
                count_gt_max,
                r.distance,
                r.movecount,
                queue.qsize(),
            )
        seenpositions.add(r.state)

        # r.calculate_locked(final_amlen)
        for r2 in r.get_next_moves(finalstate):
            if r2.state in seenpositions:
                count_seen += 1
                continue
            if max_distance is not None and r2.distance > max_distance:
                count_gt_max += 1
                continue
            r2.calculate_locked(finalstate)
            r2.history = r.history + [r]
            queue.put(r2)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    # PUZZLE_INPUT: str = clipboard.raw
    # PUZZLE_INPUT: str = PUZZLE_INPUT

    INITIAL_POSITIONS, FINAL_POSITIONS = create_node_map(PUZZLE_INPUT.splitlines())
    r = dijkstra_III(INITIAL_POSITIONS, FINAL_POSITIONS)
    print("Part 1:", r.distance)

    l2 = PUZZLE_INPUT.splitlines()
    l2 = l2[:3] + ["  #D#C#B#A#", "  #D#B#A#C#"] + l2[3:]
    INITIAL_POSITIONS2, FINAL_POSITIONS2 = create_node_map(l2)
    r2 = dijkstra_III(INITIAL_POSITIONS2, FINAL_POSITIONS2)
    print("Part 2:", r2.distance)
