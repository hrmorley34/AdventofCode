from puzzle_input import puzzle_input

NEIGHBOURS = [(1, 0), (0, 1), (-1, 0), (0, -1)]


def explore_head(map: list[list[int]], head: tuple[int, int]) -> set[tuple[int, int]]:
    height = map[head[1]][head[0]]
    positions: set[tuple[int, int]] = {head}
    while height < 9:
        height += 1
        new_positions: set[tuple[int, int]] = set()
        for x, y in positions:
            for dx, dy in NEIGHBOURS:
                if (
                    0 <= x + dx < len(map[0])
                    and 0 <= y + dy < len(map)
                    and map[y + dy][x + dx] == height
                ):
                    new_positions.add((x + dx, y + dy))
        positions = new_positions
    return positions


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    MAP = [list(map(int, line)) for line in PUZZLE_INPUT]

    print(
        "Part 1:",
        sum(
            len(explore_head(MAP, (x, y)))
            for y, line in enumerate(MAP)
            for x, i in enumerate(line)
            if i == 0
        ),
    )
