from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from itertools import count
from puzzle_input import puzzle_input
from queue import PriorityQueue
from typing import Any, Generator


@dataclass
class Node:
    neighbours: set[NodeJoin]
    id: int

    def __hash__(self) -> int:
        return hash(self.id)

    def add_join(self, distance: int, to: Node, requires: set[Node] | None = None):
        if requires is None:
            requires = set()
        j = NodeJoin(distance=distance, from_=self, to=to, requires=requires)
        self.neighbours.add(j)
        return j

    def __repr__(self) -> str:
        return (
            f"Node{self.id}(neighbours="
            + ", ".join(str(j.to.id) for j in self.neighbours)
            + ")"
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


class Runner:
    positions: dict[Node, Amphipod]
    state: frozenset[tuple[Node, Amphipod]]
    distance: int
    movecount: int
    locked: frozenset[tuple[Node, Amphipod]]

    def __init__(self, positions: dict[Node, Amphipod]) -> None:
        self.positions = positions
        self.state = self.positions_to_state(positions)
        self.distance = 0
        self.movecount = 0

    @staticmethod
    def positions_to_state(
        positions: dict[Node, Amphipod]
    ) -> frozenset[tuple[Node, Amphipod]]:
        return frozenset(positions.items())

    def calculate_locked(self, final: frozenset[tuple[Node, Amphipod]]):
        locked: set[tuple[Node, Amphipod]] = set()
        for n, am in self.state:
            if (n, am) not in final:
                continue
            elif n.id % 2 == 0:
                locked.add((n, am))
        self.locked = frozenset(locked)

    def node_occupied(self, node: Node):
        return node in self.positions

    def get_next_moves(
        self, final: frozenset[tuple[Node, Amphipod]]
    ) -> Generator[Runner, None, None]:
        for n, am in self.positions.items():
            # lock correct ones in place (don't try to move)
            if (n, am) in self.locked:
                continue

            for join in n.neighbours:
                if self.node_occupied(join.to):
                    continue
                if (join.to, am) not in final and join.to in (t[0] for t in final):
                    continue
                if any(map(self.node_occupied, join.requires)):
                    continue
                r = object.__new__(type(self))
                r.positions = self.positions.copy()
                r.positions.pop(n)
                r.positions[join.to] = am
                r.state = self.positions_to_state(r.positions)
                r.distance = self.distance + AMPHIPOD_VALUES[am] * join.distance
                r.movecount = self.movecount + join.distance
                yield r

    def __lt__(self, obj: Any) -> bool:
        if isinstance(obj, Runner):
            return self.distance < obj.distance
        return NotImplemented


def create_node_map(
    m: list[str],
) -> tuple[dict[Node, Amphipod], dict[Node, Amphipod]]:
    assert m[:2] == ["#############", "#...........#"]
    id_gen = count()

    TOWERS = [2, 4, 6, 8]
    topnodes = {x: Node(set(), id=next(id_gen)) for x in range(11) if x not in TOWERS}

    positions: dict[Node, Amphipod] = dict()
    destpositions: dict[Node, Amphipod] = dict()

    firstline, secondline = [line[3:10:2] for line in m[2:4]]
    for pos, n1, n2, dest in zip(
        TOWERS, firstline, secondline, (Amphipod.A, Amphipod.B, Amphipod.C, Amphipod.D)
    ):
        node1 = Node(set(), next(id_gen))
        node2 = Node(set(), next(id_gen))
        for x, topnode in topnodes.items():
            a = node1.add_join(abs(pos - x) + 1, topnode)
            for i in range(min(x + 1, pos), max(x - 1, pos)):
                if i in topnodes:
                    a.requires.add(topnodes[i])
            topnode.add_join(a.distance, node1, a.requires.copy())
            node2.add_join(a.distance + 1, topnode, a.requires | {node1})
            topnode.add_join(a.distance + 1, node2, a.requires | {node1})

        positions[node1] = Amphipod(n1)
        positions[node2] = Amphipod(n2)

        destpositions[node1] = dest
        destpositions[node2] = dest

    return positions, destpositions


def dijkstra_III(
    positions: dict[Node, Amphipod], final: dict[Node, Amphipod]
) -> Runner:
    queue: PriorityQueue[Runner] = PriorityQueue()
    r0 = Runner(positions.copy())
    queue.put(r0)
    seenpositions: set[frozenset[tuple[Node, Amphipod]]] = set()

    finalstate = Runner.positions_to_state(final)

    i = 0
    i2 = 0
    while True:
        r = queue.get(False)
        if r.state == finalstate:
            return r

        if r.state in seenpositions:
            i2 += 1
            continue
        i += 1
        if i % 10 ** 4 == 0:
            print(i, i2, r.distance, r.movecount, queue.qsize())
        seenpositions.add(r.state)

        r.calculate_locked(finalstate)
        for r2 in r.get_next_moves(finalstate):
            if r2.state in seenpositions:
                i2 += 1
                continue
            queue.put(r2)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    INITIAL_POSITIONS, FINAL_POSITIONS = create_node_map(PUZZLE_INPUT.splitlines())
    r = dijkstra_III(INITIAL_POSITIONS, FINAL_POSITIONS)
    print("Part 1:", r.distance)
