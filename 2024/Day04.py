from puzzle_input import puzzle_input


def grid_slice(grid: list[str], x: int, y: int, dx: int, dy: int, length: int) -> str:
    s = ""
    for d in range(length):
        cx, cy = x + d * dx, y + d * dy
        if not 0 <= cy < len(grid):
            return s
        if not 0 <= cx < len(grid[0]):
            return s
        s += grid[cy][cx]
    return s


DIRECTIONS = [
    (-1, 1),
    (0, 1),
    (1, 1),
    (1, 0),
    # (1, -1),
    # (0, -1),
    # (-1, -1),
    # (-1, 0),
]


def count_occurrences(grid: list[str], word: str) -> int:
    WORDS = {word, word[::-1]}
    count = 0
    for y in range(len(grid)):
        for x in range(len(grid[0])):
            for dx, dy in DIRECTIONS:
                count += grid_slice(grid, x, y, dx, dy, len(word)) in WORDS
    return count


DIAGONALS = [
    (-1, 1),
    (1, 1),
    (1, -1),
    (-1, -1),
]


def count_x_occurrences(grid: list[str]) -> int:
    count = 0
    for y in range(1, len(grid) - 1):
        for x in range(1, len(grid[0]) - 1):
            if grid[y][x] != "A":
                continue
            # count += (
            #     # sum == 0 means no MASes
            #     # sum == 1 means only one diagonal
            #     # sum == 2 means two neighbouring diagonals, because it cannot be
            #     # opposite diagonals simultaneously
            #     sum(
            #         grid[y - dy][x - dx] == "M" and grid[y + dy][x + dx] == "S"
            #         for dx, dy in X_DIRECTIONS
            #     )
            #     == 2
            # )
            diagonals = [grid[y + dy][x + dx] for dx, dy in DIAGONALS]
            count += (
                set(diagonals) == {"M", "S"}
                and diagonals[0] != diagonals[2]
                and diagonals[1] != diagonals[3]
                # both diagonals contain an M and an S (so must be MAS forwards
                # or backwards)
            )
    return count


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    print("Part 1:", count_occurrences(PUZZLE_INPUT, "XMAS"))

    print("Part 2:", count_x_occurrences(PUZZLE_INPUT))
