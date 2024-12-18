from collections.abc import Collection
from queue import Empty, PriorityQueue

from puzzle_input import puzzle_input

# SIZE = 7
# P1SIZE = 12
SIZE = 71
P1SIZE = 1024

NEIGHBOURS = [(1, 0), (0, 1), (-1, 0), (0, -1)]


def solve_maze(
    coords: Collection[tuple[int, int]], size: int
) -> tuple[int, set[tuple[int, int]]]:
    q: PriorityQueue[tuple[float, int, tuple[int, int], set[tuple[int, int]]]] = (
        PriorityQueue()
    )
    seen: set[tuple[int, int]] = set()
    q.put((size - 1, 0, (0, 0), {(0, 0)}))
    while True:
        _, d, (x, y), s = q.get_nowait()
        if (x, y) in seen:
            continue
        elif (x, y) == (size - 1, size - 1):
            return d, s
        seen.add((x, y))
        for dx, dy in NEIGHBOURS:
            nx, ny = x + dx, y + dy
            if (
                0 <= nx < size
                and 0 <= ny < size
                and (nx, ny) not in coords
                and (nx, ny) not in seen
            ):
                q.put_nowait(
                    (
                        ((size - 1 - nx) ** 2 + (size - 1 - ny) ** 2) ** 0.5 + d + 1,
                        d + 1,
                        (nx, ny),
                        s | {(nx, ny)},
                    )
                )


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    COORDS = [(int(a), int(b)) for a, b in (s.split(",") for s in PUZZLE_INPUT)]

    d, s = solve_maze(COORDS[:P1SIZE], SIZE)
    print("Part 1:", d)
    for bytecount in range(P1SIZE + 1, len(COORDS)):
        if COORDS[bytecount - 1] in s:
            try:
                d, s = solve_maze(COORDS[:bytecount], SIZE)
            except Empty:
                print("Part 2:", ",".join(map(str, COORDS[bytecount - 1])))
                break
