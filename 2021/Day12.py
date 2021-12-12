from __future__ import annotations

from collections import defaultdict
from copy import deepcopy
from queue import Queue
from typing import NewType, TypeVar, cast
from puzzle_input import puzzle_input


Node = NewType("Node", str)


def puzzlei_to_map(puzzlei: list[tuple[str, str]]) -> dict[Node, set[Node]]:
    d: defaultdict[Node, set[Node]] = defaultdict(set)
    for a, b in puzzlei:
        d[Node(a)].add(Node(b))
        d[Node(b)].add(Node(a))
    return dict(d)


def is_small(n: Node) -> bool:
    return n.lower() == n


def trim_map(
    map: dict[Node, set[Node]], start: Node, end: Node, *, trim_single: bool = False
) -> dict[Node, set[Node]]:
    map[end].clear()
    again = True
    while again:
        again = False
        for key in list(map):
            if (
                trim_single
                and is_small(key)
                and len(map[key]) == 1
                and is_small(next(iter(map[key])))
            ):
                map.pop(key)
                again = True
            else:
                for k in set(map[key]):
                    if k == start or k not in map:
                        map[key].remove(k)
    return map


class Runner:
    current: Node
    route: list[Node]

    def __init__(self, current: Node, route: list[Node] | None = None) -> None:
        if route is None:
            route = [current]
        self.current = current
        self.route = route

    def copy(self) -> Runner:
        return Runner(self.current, self.route.copy())

    def can_go_to(self, node: Node) -> bool:
        return not (is_small(node) and node in self.route)

    def move_to(self, node: Node):
        self.route.append(node)
        self.current = node

    def __repr__(self) -> str:
        return type(self).__name__ + "(" + ",".join(self.route) + ")"


class Runner2(Runner):
    has_done_twice: bool

    def __init__(
        self,
        current: Node,
        route: list[Node] | None = None,
        has_done_twice: bool = False,
    ) -> None:
        super().__init__(current, route=route)
        self.has_done_twice = has_done_twice

    def can_go_to(self, node: Node) -> bool:
        return not (
            is_small(node)
            and (
                node in self.route
                if self.has_done_twice
                else self.route.count(node) >= 2
            )
        )

    def copy(self) -> Runner2:
        return Runner2(self.current, self.route.copy(), self.has_done_twice)

    def move_to(self, node: Node):
        if is_small(node) and node in self.route:
            self.has_done_twice = True
        return super().move_to(node)


RunnerT = TypeVar("RunnerT", bound=Runner)


def run(
    m: dict[Node, set[Node]],
    start: Node,
    end: Node,
    *,
    RunnerType: type[RunnerT] = Runner
) -> list[RunnerT]:
    queue: Queue[RunnerT] = Queue()
    queue.put(RunnerType(start))
    output: list[RunnerT] = list()
    while not queue.empty():
        r = queue.get()
        if r.current == end:
            output.append(r)
            continue
        for node in m[r.current]:
            if r.can_go_to(node):
                copyr = r.copy()
                copyr.move_to(node)
                queue.put(copyr)
    return output


if __name__ == "__main__":
    PUZZLE_INPUT = [
        cast(tuple[str, str], tuple(line.split("-")))
        for line in puzzle_input().splitlines()
    ]
    m = puzzlei_to_map(PUZZLE_INPUT)
    START = Node("start")
    END = Node("end")
    m1 = trim_map(deepcopy(m), START, END, trim_single=True)
    m2 = trim_map(deepcopy(m), START, END, trim_single=False)

    print("Part 1:", len(run(m1, START, END)))
    print("Part 2:", len(run(m2, START, END, RunnerType=Runner2)))
