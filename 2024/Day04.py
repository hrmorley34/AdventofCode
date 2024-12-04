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
    (1, -1),
    (0, -1),
    (-1, -1),
    (-1, 0),
]


def count_occurrences(grid: list[str], word: str) -> int:
    count = 0
    for y in range(len(grid)):
        for x in range(len(grid[0])):
            for dx, dy in DIRECTIONS:
                count += grid_slice(grid, x, y, dx, dy, len(word)) == word
    return count


X_DIRECTIONS = [
    ((-1, 1), (1, 1)),
    ((1, 1), (1, -1)),
    ((1, -1), (-1, -1)),
    ((-1, -1), (-1, 1)),
]


def count_x_occurrences(grid: list[str]) -> int:
    count = 0
    for y in range(len(grid)):
        for x in range(len(grid[0])):
            for (dx1, dy1), (dx2, dy2) in X_DIRECTIONS:
                count += (
                    grid_slice(grid, x - dx1, y - dy1, dx1, dy1, 3) == "MAS"
                    and grid_slice(grid, x - dx2, y - dy2, dx2, dy2, 3) == "MAS"
                )
    return count


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    print("Part 1:", count_occurrences(PUZZLE_INPUT, "XMAS"))

    print("Part 2:", count_x_occurrences(PUZZLE_INPUT))
