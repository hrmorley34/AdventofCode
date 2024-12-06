from collections.abc import Generator

from puzzle_input import puzzle_input

DIRECTIONS = [(1, 0), (0, 1), (-1, 0), (0, -1)]
START_CHAR = "^"
START_DIR = 3


def explore_map(map: list[str]) -> Generator[tuple[int, int], None, None]:
    ((y, start_row),) = ((i, r) for i, r in enumerate(map) if START_CHAR in r)
    x = start_row.index(START_CHAR)
    dir = START_DIR

    while 0 <= y < len(map) and 0 <= x < len(map[0]):
        yield (x, y)
        dx, dy = DIRECTIONS[dir]
        while (
            0 <= x + dx < len(map)
            and 0 <= y + dy < len(map[0])
            and map[y + dy][x + dx] == "#"
        ):
            dir = (dir + 1) % len(DIRECTIONS)
            dx, dy = DIRECTIONS[dir]
        x, y = x + dx, y + dy


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    print("Part 1:", len(set(explore_map(PUZZLE_INPUT))))
