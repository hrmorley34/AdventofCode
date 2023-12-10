from collections import defaultdict

from puzzle_input import puzzle_input

DIRECTIONS = {
    "|": ((0, -1), (0, 1)),
    "-": ((-1, 0), (1, 0)),
    "L": ((1, 0), (0, -1)),
    "J": ((-1, 0), (0, -1)),
    "7": ((-1, 0), (0, 1)),
    "F": ((1, 0), (0, 1)),
}


def find_connections(
    lines: list[str]
) -> tuple[tuple[int, int], defaultdict[tuple[int, int], set[tuple[int, int]]]]:
    connections: defaultdict[tuple[int, int], set[tuple[int, int]]] = defaultdict(set)
    reverse_connections: defaultdict[
        tuple[int, int], set[tuple[int, int]]
    ] = defaultdict(set)
    start: tuple[int, int] | None = None
    for y, line in enumerate(lines):
        for x, c in enumerate(line):
            pos = (x, y)
            if c == "S":
                start = pos
                continue
            vecs = DIRECTIONS.get(c)
            if vecs is None:
                continue
            for dx, dy in vecs:
                dpos = (x + dx, y + dy)
                if (
                    dpos[0] >= len(line)
                    or dpos[0] < 0
                    or dpos[1] >= len(lines)
                    or dpos[1] < 0
                ):
                    continue
                connections[pos].add(dpos)
                reverse_connections[dpos].add(pos)
    assert start is not None
    assert len(reverse_connections[start]) == 2
    connections[start].update(reverse_connections[start])

    return start, connections


def trace_loop(
    start: tuple[int, int],
    connections: defaultdict[tuple[int, int], set[tuple[int, int]]],
) -> int:
    distance = 1
    pos1, pos2 = connections[start]
    prev1 = prev2 = start
    while pos1 != pos2 and pos1 != prev2:
        d1a, d1b = connections[pos1]
        if d1a == prev1:
            prev1, pos1 = pos1, d1b
        else:
            assert d1b == prev1
            prev1, pos1 = pos1, d1a
        d2a, d2b = connections[pos2]
        if d2a == prev2:
            prev2, pos2 = pos2, d2b
        else:
            assert d2b == prev2
            prev2, pos2 = pos2, d2a
        distance += 1
    return distance


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    start, conn = find_connections(PUZZLE_INPUT)
    print("Part 1:", trace_loop(start, conn))
