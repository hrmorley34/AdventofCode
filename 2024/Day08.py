from collections import defaultdict
from collections.abc import Mapping

from puzzle_input import puzzle_input


def get_nodes(grid: list[str]) -> Mapping[str, set[tuple[int, int]]]:
    d: defaultdict[str, set[tuple[int, int]]] = defaultdict(set)
    for y, line in enumerate(grid):
        for x, c in enumerate(line):
            if c != ".":
                d[c].add((x, y))
    return d


def get_antinodes(
    nodes: set[tuple[int, int]], max_x: int, max_y: int, single: bool
) -> set[tuple[int, int]]:
    s: set[tuple[int, int]] = set()
    for ax, ay in nodes:
        for bx, by in nodes:
            if (ax, ay) == (bx, by):
                continue
            if not single:
                s.add((ax, ay))
            dx, dy = ax - bx, ay - by
            px, py = ax + dx, ay + dy
            while 0 <= px < max_x and 0 <= py < max_y:
                s.add((px, py))
                if single:
                    break
                px += dx
                py += dy
    return s


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    MAX_X, MAX_Y = len(PUZZLE_INPUT[0]), len(PUZZLE_INPUT)

    NODES = get_nodes(PUZZLE_INPUT)
    ANTINODES_SINGLE = set[tuple[int, int]]().union(
        *(get_antinodes(n, MAX_X, MAX_Y, single=True) for n in NODES.values())
    )
    print("Part 1:", len(ANTINODES_SINGLE))

    ANTINODES_ALL = set[tuple[int, int]]().union(
        *(get_antinodes(n, MAX_X, MAX_Y, single=False) for n in NODES.values())
    )
    print("Part 2:", len(ANTINODES_ALL))
