from __future__ import annotations

from dataclasses import dataclass
from heapq import heappop, heappush

from puzzle_input import puzzle_input

ADJ = [(-1, 0), (0, -1), (1, 0), (0, 1)]


@dataclass
class Node:
    pos: tuple[int, int]
    neighbours: dict[Node, int]

    def __hash__(self) -> int:
        return hash(self.pos)


@dataclass
class SearchState:
    distance: int
    node: Node
    visited: set[Node]

    def __lt__(self, o: SearchState):
        return self.distance < o.distance


@dataclass
class Trail:
    nodes: list[Node]
    start: Node
    end: Node

    @classmethod
    def from_lines(cls, lines: list[str]):
        assert lines[0].count(".") == 1
        assert lines[-1].count(".") == 1
        assert all(line[0] == "#" and line[-1] == "#" for line in lines)

        nodes: list[Node] = [
            Node((x, y), {})
            for y in (0, len(lines) - 1)
            for x in range(1, len(lines[0]) - 1)
            if lines[y][x] == "."
        ]
        start, end = nodes
        assert end.pos[1] > start.pos[1]
        for y in range(1, len(lines) - 1):
            last_right: Node | None = None
            next_left: Node | None = None
            for x in range(1, len(lines[0]) - 1):
                c = lines[y][x]
                if c == "#":
                    last_right = next_left = None
                    continue
                neighbours = [lines[y + dy][x + dx] != "#" for dx, dy in ADJ]
                count = neighbours.count(True)

                assert count > 0, (x, y, neighbours)
                if count == 1:
                    # dead end
                    n = Node((x, y), {})
                elif neighbours in (
                    [True, False, True, False],
                    [False, True, False, True],
                ):
                    # straight line
                    if c == "<":
                        last_right = None
                    elif c == ">":
                        next_left = None
                    continue
                else:
                    assert c == ".", c  # not arrow
                    # junction or corner
                    n = Node((x, y), {})

                if last_right is not None:
                    assert n.pos[1] == last_right.pos[1]
                    last_right.neighbours[n] = n.pos[0] - last_right.pos[0]
                if next_left is not None:
                    assert n.pos[1] == next_left.pos[1]
                    n.neighbours[next_left] = n.pos[0] - next_left.pos[0]
                nodes.append(n)
                last_right = next_left = n

        nodes_by_pos: dict[tuple[int, int], Node] = {n.pos: n for n in nodes}
        for x in range(1, len(lines[0]) - 1):
            last_down: Node | None = None
            next_up: Node | None = None
            for y in range(0, len(lines)):
                c = lines[y][x]
                if c == "#":
                    last_down = next_up = None
                    continue

                n = nodes_by_pos.get((x, y))
                if n is None:
                    if c == "^":
                        last_down = None
                    elif c == "v":
                        next_up = None
                    continue

                if last_down is not None:
                    assert n.pos[0] == last_down.pos[0]
                    last_down.neighbours[n] = n.pos[1] - last_down.pos[1]
                if next_up is not None:
                    assert n.pos[0] == next_up.pos[0]
                    n.neighbours[next_up] = n.pos[1] - next_up.pos[1]
                last_down = next_up = n

        return cls(nodes, start=start, end=end)

    def find_longest_path(self) -> int:
        states: list[SearchState] = [SearchState(0, self.start, {self.start})]
        end_states: list[SearchState] = []
        while states:
            state = heappop(states)
            if state.node == self.end:
                end_states.append(state)
                continue
            for neighbour, d in state.node.neighbours.items():
                if neighbour not in state.visited:
                    heappush(
                        states,
                        SearchState(
                            state.distance + d, neighbour, state.visited | {neighbour}
                        ),
                    )
        return end_states[-1].distance


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    trail = Trail.from_lines(PUZZLE_INPUT)

    print("Part 1:", trail.find_longest_path())
