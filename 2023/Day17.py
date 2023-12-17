from dataclasses import dataclass
from heapq import heappop, heappush

from puzzle_input import puzzle_input


@dataclass
class DijkstraNode:
    loss: int
    pos: tuple[int, int]
    direction: tuple[int, int]

    via: "DijkstraNode | None"

    @property
    def sortkey(self):
        return self.loss

    def __lt__(self, o: "DijkstraNode") -> bool:
        return self.sortkey < o.sortkey


def do_dijkstra(
    nummap: list[list[int]],
    start: tuple[int, int],
    end: tuple[int, int],
    turn_range: tuple[int, int],
) -> int:
    dim = len(nummap[0]), len(nummap)
    q = [DijkstraNode(0, start, (0, 1), None), DijkstraNode(0, start, (1, 0), None)]
    visited: set[tuple[tuple[int, int], tuple[int, int]]] = set()
    while q:
        n = heappop(q)
        if n.pos == end:
            return n.loss
        if (n.pos, n.direction) in visited:
            continue
        visited.add((n.pos, n.direction))
        new_loss = 0
        for i in range(1, turn_range[1] + 1):
            new_pos = n.pos[0] + n.direction[0] * i, n.pos[1] + n.direction[1] * i
            if not (0 <= new_pos[0] < dim[0] and 0 <= new_pos[1] < dim[1]):
                break
            new_loss += nummap[new_pos[1]][new_pos[0]]
            if i >= turn_range[0]:
                heappush(
                    q,
                    DijkstraNode(
                        n.loss + new_loss,
                        new_pos,
                        (-n.direction[1], -n.direction[0]),
                        n,
                    ),
                )
                heappush(
                    q,
                    DijkstraNode(
                        n.loss + new_loss,
                        new_pos,
                        (+n.direction[1], +n.direction[0]),
                        n,
                    ),
                )
    raise RuntimeError


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    heatloss = [list(map(int, line)) for line in PUZZLE_INPUT]

    print(
        "Part 1:",
        do_dijkstra(
            heatloss, (0, 0), (len(heatloss[0]) - 1, len(heatloss) - 1), (1, 3)
        ),
    )

    print(
        "Part 2:",
        do_dijkstra(
            heatloss, (0, 0), (len(heatloss[0]) - 1, len(heatloss) - 1), (4, 10)
        ),
    )
