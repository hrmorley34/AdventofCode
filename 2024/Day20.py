from collections import Counter
from collections.abc import Iterable
from queue import PriorityQueue

from puzzle_input import puzzle_input

NEIGHBOURS = [(1, 0), (0, 1), (-1, 0), (0, -1)]


def get_cheats(max_length: int) -> set[tuple[tuple[int, int], int]]:
    return {
        ((negx * dx, negy * (length - dx)), length)
        for length in range(2, max_length + 1)
        for dx in range(0, length + 1)
        for negx in (1, -1)
        for negy in (1, -1)
    }


CHEATS1 = get_cheats(2)
CHEATS2 = get_cheats(20)


def dijkstra(
    grid: list[str], start: tuple[int, int], end: tuple[int, int]
) -> dict[tuple[int, int], int]:
    q: PriorityQueue[tuple[int, tuple[int, int]]] = PriorityQueue()
    d: dict[tuple[int, int], int] = {}
    q.put_nowait((0, start))
    while True:
        w, (x, y) = q.get_nowait()
        if (x, y) == end:
            d[x, y] = w
            return d
        if (x, y) in d:
            continue
        d[x, y] = w
        for dx, dy in NEIGHBOURS:
            nx, ny = x + dx, y + dy
            if (nx, ny) in d:
                continue
            if 0 <= ny < len(grid) and 0 <= nx < len(grid[ny]) and grid[ny][nx] != "#":
                q.put_nowait((w + 1, (nx, ny)))


def dijkstra_cheat(
    grid: list[str], cheats: Iterable[tuple[tuple[int, int], int]]
) -> tuple[int, Counter[int]]:
    START = next(
        iter((line.index("S"), i) for i, line in enumerate(grid) if "S" in line)
    )
    END = next(iter((line.index("E"), i) for i, line in enumerate(grid) if "E" in line))
    c: Counter[int] = Counter()
    base_d = dijkstra(grid, START, END)
    rev_d = dijkstra(grid, END, START)
    for (x, y), w in base_d.items():
        for (dx, dy), dd in cheats:
            nx, ny, nw = x + dx, y + dy, w + dd
            if not (
                0 <= ny < len(grid) and 0 <= nx < len(grid[ny]) and grid[ny][nx] != "#"
            ):
                continue
            if (nx, ny) not in rev_d:
                new_d = dijkstra(grid, (nx, ny), END)
                distance = nw + new_d[END]
            else:
                distance = nw + rev_d[nx, ny]
            if distance < base_d[END]:
                c[base_d[END] - distance] += 1
    return base_d[END], c


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    print(
        "Part 1:",
        sum(v for c, v in dijkstra_cheat(PUZZLE_INPUT, CHEATS1)[1].items() if c >= 100),
    )
    print(
        "Part 2:",
        sum(v for c, v in dijkstra_cheat(PUZZLE_INPUT, CHEATS2)[1].items() if c >= 100),
    )
