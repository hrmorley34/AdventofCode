from collections.abc import Generator

from puzzle_input import puzzle_input

DIRECTIONS = [(1, 0), (0, 1), (-1, 0), (0, -1)]
START_CHAR = "^"
START_DIR = 3


def explore_map(
    map: list[str],
    start_pos: tuple[int, int] | None = None,
    start_dir: int | None = None,
) -> Generator[tuple[tuple[int, int], int], None, None]:
    if start_pos is None:
        ((y, start_row),) = ((i, r) for i, r in enumerate(map) if START_CHAR in r)
        x = start_row.index(START_CHAR)
    else:
        x, y = start_pos
    if start_dir is None:
        dir = START_DIR
    else:
        dir = start_dir % len(DIRECTIONS)

    while 0 <= y < len(map) and 0 <= x < len(map[0]):
        dx, dy = DIRECTIONS[dir]
        while (
            0 <= x + dx < len(map)
            and 0 <= y + dy < len(map[0])
            and map[y + dy][x + dx] == "#"
        ):
            dir = (dir + 1) % len(DIRECTIONS)
            dx, dy = DIRECTIONS[dir]

        yield (x, y), dir

        x, y = x + dx, y + dy


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    positions = set[tuple[int, int]]()
    pos_dirs = set[tuple[tuple[int, int], int]]()
    loop_creators = set[tuple[int, int]]()
    for (x, y), dir in explore_map(PUZZLE_INPUT):
        positions.add((x, y))
        pos_dirs.add(((x, y), dir))

        dx, dy = DIRECTIONS[dir]
        if (
            0 <= x + dx < len(PUZZLE_INPUT)
            and 0 <= y + dy < len(PUZZLE_INPUT[0])
            and PUZZLE_INPUT[y + dy][x + dx] != "#"
            # and we insert a wall here
            and (x + dx, y + dy) not in positions
            # haven't previously passed through this space
        ):
            new_map = [
                "".join(
                    "#" if ix == x + dx and iy == y + dy else c
                    for ix, c in enumerate(line)
                )
                for iy, line in enumerate(PUZZLE_INPUT)
            ]
            sub_pos_dirs = set[tuple[tuple[int, int], int]]()
            for pd in explore_map(new_map, (x, y), dir):
                if pd in sub_pos_dirs:
                    loop_creators.add((x + dx, y + dy))
                    break
                sub_pos_dirs.add(pd)

    print("Part 1:", len(positions))
    print("Part 2:", len(loop_creators))
