from puzzle_input import puzzle_input


def count_visible(grid: list[list[int]]) -> tuple[int, int]:
    w, h = len(grid[0]), len(grid)
    visible_grid: list[list[bool]] = [[False for _ in range(w)] for _ in range(h)]
    view_distances: list[list[int]] = [[1 for _ in range(w)] for _ in range(h)]
    for y in range(h):
        # right
        v = -1
        distances = [0 for _ in range(10)]
        for x in range(w):
            c = grid[y][x]
            if c > v:
                visible_grid[y][x] = True
                v = c
            view_distances[y][x] *= distances[c]
            for i in range(0, c + 1):
                distances[i] = 0
            for i in range(10):
                distances[i] += 1

        # left
        v = -1
        distances = [0 for _ in range(10)]
        for x in range(w - 1, -1, -1):
            c = grid[y][x]
            if c > v:
                visible_grid[y][x] = True
                v = c
            view_distances[y][x] *= distances[c]
            for i in range(0, c + 1):
                distances[i] = 0
            for i in range(10):
                distances[i] += 1

    for x in range(w):
        # down
        v = -1
        distances = [0 for _ in range(10)]
        for y in range(h):
            c = grid[y][x]
            if c > v:
                visible_grid[y][x] = True
                v = c
            view_distances[y][x] *= distances[c]
            for i in range(0, c + 1):
                distances[i] = 0
            for i in range(10):
                distances[i] += 1

        # up
        v = -1
        distances = [0 for _ in range(10)]
        for y in range(h - 1, -1, -1):
            c = grid[y][x]
            if c > v:
                visible_grid[y][x] = True
                v = c
            view_distances[y][x] *= distances[c]
            for i in range(0, c + 1):
                distances[i] = 0
            for i in range(10):
                distances[i] += 1

    # for line in visible_grid:
    #     print("".join("X" if b else " " for b in line))

    # for line in view_distances:
    #     print("".join(str(v).rjust(2) for v in line))

    return sum(map(sum, visible_grid)), max(max(row) for row in view_distances)


if __name__ == "__main__":
    PUZZLE_INPUT = [[int(i) for i in line] for line in puzzle_input().splitlines()]

    p1, p2 = count_visible(PUZZLE_INPUT)
    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")
