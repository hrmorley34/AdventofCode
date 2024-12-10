from puzzle_input import puzzle_input

NEIGHBOURS = [(1, 0), (0, 1), (-1, 0), (0, -1)]


def explore_head(
    map: list[list[int]], head: tuple[int, int]
) -> set[tuple[tuple[int, int], ...]]:
    height = map[head[1]][head[0]]
    paths: set[tuple[tuple[int, int], ...]] = {(head,)}
    while height < 9:
        height += 1
        new_paths: set[tuple[tuple[int, int], ...]] = set()
        for path in paths:
            x, y = path[-1]
            for dx, dy in NEIGHBOURS:
                if (
                    0 <= x + dx < len(map[0])
                    and 0 <= y + dy < len(map)
                    and map[y + dy][x + dx] == height
                ):
                    new_paths.add(path + ((x + dx, y + dy),))
        paths = new_paths
    return paths


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    MAP = [list(map(int, line)) for line in PUZZLE_INPUT]

    print(
        "Part 1:",
        sum(
            len({path[-1] for path in explore_head(MAP, (x, y))})
            for y, line in enumerate(MAP)
            for x, i in enumerate(line)
            if i == 0
        ),
    )
    print(
        "Part 2:",
        sum(
            len(explore_head(MAP, (x, y)))
            for y, line in enumerate(MAP)
            for x, i in enumerate(line)
            if i == 0
        ),
    )
