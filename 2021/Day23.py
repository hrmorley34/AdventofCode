from __future__ import annotations

from collections import defaultdict
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
            f"Node{self.id}(neighbours={{"
            + ", ".join(str(j.to.id) for j in self.neighbours)
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


def amlen_calc(
    state: frozenset[tuple[Node, Amphipod]],
    exclude: dict[Amphipod, dict[int, Node]] | None = None,
) -> dict[Amphipod, dict[int, Node]]:
    amlen: defaultdict[Amphipod, dict[int, Node]] = defaultdict(dict)
    for n, am in state:
        if exclude is not None and (am not in exclude or n not in exclude[am].values()):
            continue
        joinit = iter(n.neighbours)
        requires = next(joinit).requires
        for j in joinit:
            requires &= j.requires
        assert len(requires) not in amlen[am]
        amlen[am][len(requires)] = n
    return dict(amlen)


class RunnerConfig:
    sort_with_lockcount: bool = True


class Runner:
    positions: dict[Node, Amphipod]
    state: frozenset[tuple[Node, Amphipod]]
    distance: int
    movecount: int
    locked: frozenset[tuple[Node, Amphipod]]
    config: RunnerConfig

    def __init__(self, positions: dict[Node, Amphipod], config: RunnerConfig) -> None:
        self.positions = positions
        self.state = self.positions_to_state(positions)
        self.distance = 0
        self.movecount = 0
        self.config = config

    @staticmethod
    def positions_to_state(
        positions: dict[Node, Amphipod]
    ) -> frozenset[tuple[Node, Amphipod]]:
        return frozenset(positions.items())

    def calculate_locked(self, final_amlen: dict[Amphipod, dict[int, Node]]):
        amlen = amlen_calc(self.state, final_amlen)

        locked: set[tuple[Node, Amphipod]] = set()
        for am, d in amlen.items():
            d_final = final_amlen[am]
            for i in reversed(sorted(d_final)):
                if i not in d:
                    break
                assert d_final[i] == d[i]
                locked.add((d[i], am))
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


def create_node_map(
    m: list[str],
) -> tuple[dict[Node, Amphipod], dict[Node, Amphipod]]:
    assert m[:2] == ["#############", "#...........#"]
    id_gen = count()

    TOWERS = [2, 4, 6, 8]
    topnodes = {x: Node(set(), id=next(id_gen)) for x in range(11) if x not in TOWERS}

    positions: dict[Node, Amphipod] = dict()
    destpositions: dict[Node, Amphipod] = dict()

    lines = [line[3:10:2] for line in m[2:-1]]
    for pos, dest, *nx in zip(
        TOWERS, (Amphipod.A, Amphipod.B, Amphipod.C, Amphipod.D), *lines
    ):
        nx: list[str]
        nodes = [Node(set(), next(id_gen)) for _ in range(len(nx))]
        for x, topnode in topnodes.items():
            a = nodes[0].add_join(abs(pos - x) + 1, topnode)
            for i in range(min(x + 1, pos), max(x - 1, pos)):
                if i in topnodes:
                    a.requires.add(topnodes[i])
            topnode.add_join(a.distance, nodes[0], a.requires.copy())
            for i, node in enumerate(nodes[1:], 1):
                node.add_join(a.distance + i, topnode, a.requires | set(nodes[:i]))
                topnode.add_join(a.distance + i, node, a.requires | set(nodes[:i]))

        for node, n in zip(nodes, nx):
            positions[node] = Amphipod(n)
            destpositions[node] = dest

    return positions, destpositions


def dijkstra_III(
    positions: dict[Node, Amphipod], final: dict[Node, Amphipod]
) -> Runner:
    conf = RunnerConfig()

    finalstate = Runner.positions_to_state(final)
    final_amlen = amlen_calc(finalstate)
    r0 = Runner(positions.copy(), conf)
    r0.calculate_locked(final_amlen)

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
                old_queue = queue
                queue = PriorityQueue()
                queue.put(r)
                while not old_queue.empty():
                    v = old_queue.get(False)
                    if v.distance <= max_distance:
                        queue.put(v)
                    else:
                        count_gt_max += 1
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
            r2.calculate_locked(final_amlen)
            queue.put(r2)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    # PUZZLE_INPUT: str = clipboard.raw
    # PUZZLE_INPUT: str = PUZZLE_INPUT
    INITIAL_POSITIONS, FINAL_POSITIONS = create_node_map(PUZZLE_INPUT.splitlines())
    r = dijkstra_III(INITIAL_POSITIONS, FINAL_POSITIONS)
    print("Part 1:", r.distance)

    # l2 = PUZZLE_INPUT.splitlines()
    # l2 = l2[:3] + ["  #D#C#B#A#", "  #D#B#A#C#"] + l2[3:]
    # INITIAL_POSITIONS2, FINAL_POSITIONS2 = create_node_map(l2)
    # r = dijkstra_III(INITIAL_POSITIONS2, FINAL_POSITIONS2)
    # print("Part 2:", r.distance)
