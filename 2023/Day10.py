from collections import defaultdict

from puzzle_input import puzzle_input

PIPES = {
    "|": ((0, -1), (0, 1)),
    "-": ((-1, 0), (1, 0)),
    "L": ((1, 0), (0, -1)),
    "J": ((-1, 0), (0, -1)),
    "7": ((-1, 0), (0, 1)),
    "F": ((1, 0), (0, 1)),
}
DIRECTIONS = [(1, 0), (0, 1), (-1, 0), (0, -1)]


def in_range(pos: tuple[int, int], dim: tuple[int, int]) -> bool:
    return pos[0] >= 0 and pos[0] < dim[0] and pos[1] >= 0 and pos[1] < dim[1]


def find_connections(
    lines: list[str], dim: tuple[int, int]
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
            vecs = PIPES.get(c)
            if vecs is None:
                continue
            for dx, dy in vecs:
                dpos = (x + dx, y + dy)
                if not in_range(dpos, dim):
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
) -> tuple[int, dict[tuple[int, int], tuple[int, int]]]:
    distance = 1
    pos1, pos2 = connections[start]
    prev1 = prev2 = start
    true_connections: dict[tuple[int, int], tuple[int, int]] = {
        prev1: pos1,
        pos2: prev2,
    }
    while pos1 != pos2 and pos1 != prev2:
        d1a, d1b = connections[pos1]
        if d1a == prev1:
            prev1, pos1 = pos1, d1b
        else:
            assert d1b == prev1
            prev1, pos1 = pos1, d1a
        true_connections[prev1] = pos1
        d2a, d2b = connections[pos2]
        if d2a == prev2:
            prev2, pos2 = pos2, d2b
        else:
            assert d2b == prev2
            prev2, pos2 = pos2, d2a
        true_connections[pos2] = prev2
        distance += 1
    true_connections[prev1] = pos1
    if pos1 == pos2:
        true_connections[pos2] = prev2
    return distance, true_connections


def flood(
    s: set[tuple[int, int]],
    wall: set[tuple[int, int]],
    illegal: set[tuple[int, int]],
    dim: tuple[int, int],
) -> bool:
    q: set[tuple[int, int]] = set(s)
    while q:
        pos = x, y = q.pop()
        s.add(pos)
        for dx, dy in DIRECTIONS:
            npos = (x + dx, y + dy)
            if not in_range(npos, dim):
                return False
            if npos in s:
                continue
            elif npos in wall:
                continue
            elif npos in illegal:
                assert False, npos
            elif npos in q:
                continue
            q.add(npos)
    return True


def join_up(dold: tuple[int, int], dnew: tuple[int, int]) -> set[tuple[int, int]]:
    if dold == dnew:
        return {dnew}
    else:
        djoin = (dold[0] + dnew[0], dold[1] + dnew[1])
        assert all(djoin) or dold == (0, 0), (dold, djoin, dnew)
        return {dold, djoin, dnew}


def fill_sides(
    start: tuple[int, int],
    true_connections: dict[tuple[int, int], tuple[int, int]],
    dim: tuple[int, int],
) -> int:
    walls = set(true_connections)

    left_positions: set[tuple[int, int]] = set()
    right_positions: set[tuple[int, int]] = set()
    pos, newp = start, true_connections[start]
    pldpos = prdpos = (0, 0)
    while pldpos == (0, 0) or pos != start:
        dx, dy = (newp[0] - pos[0], newp[1] - pos[1])
        bldpos = (+dy, -dx)
        brdpos = (-dy, +dx)

        for ldx, ldy in join_up(pldpos, bldpos):
            lpos = (pos[0] + ldx, pos[1] + ldy)
            if in_range(lpos, dim) and lpos not in walls:
                left_positions.add(lpos)
        for rdx, rdy in join_up(prdpos, brdpos):
            rpos = (pos[0] + rdx, pos[1] + rdy)
            if in_range(rpos, dim) and rpos not in walls:
                right_positions.add(rpos)

        pos, newp = newp, true_connections[newp]
        pldpos, prdpos = bldpos, brdpos

    assert not (left_positions & right_positions)

    bl = flood(left_positions, walls, right_positions, dim)
    br = flood(right_positions, walls, left_positions, dim)

    if bl:
        assert not br
        return len(left_positions)
    else:
        assert br
        return len(right_positions)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    DIM = (len(PUZZLE_INPUT[0]), len(PUZZLE_INPUT))

    start, conn = find_connections(PUZZLE_INPUT, DIM)
    length, tconn = trace_loop(start, conn)
    print("Part 1:", length)

    print("Part 2:", fill_sides(start, tconn, DIM))
