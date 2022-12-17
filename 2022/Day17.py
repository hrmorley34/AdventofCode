from itertools import cycle
from typing import Iterator


def vadd(a: tuple[int, int], b: tuple[int, int]) -> tuple[int, int]:
    return (a[0] + b[0], a[1] + b[1])


class Rock:
    positions: set[tuple[int, int]]

    def __init__(self, positions: set[tuple[int, int]]) -> None:
        self.positions = positions

    def collide(self, d_position: tuple[int, int], other: set[tuple[int, int]]):
        return any(vadd(p, d_position) in other for p in self.positions)

    def wall_collide(self, d_position: tuple[int, int], walls: tuple[int, int]):
        return not all(
            walls[0] < p[0] + d_position[0] < walls[1] for p in self.positions
        )


ROCKS = [
    # All have zeroes aligned with left-most and bottom-most point
    # +x is right; +y is up
    Rock({(0, 0), (1, 0), (2, 0), (3, 0)}),
    Rock({(1, 2), (2, 1), (1, 1), (0, 1), (1, 0)}),
    Rock({(2, 2), (2, 1), (2, 0), (1, 0), (0, 0)}),
    Rock({(0, 0), (0, 1), (0, 2), (0, 3)}),
    Rock({(0, 0), (1, 0), (0, 1), (1, 1)}),
]


class GenericFallingBlockSimulator:
    grid: set[tuple[int, int]]
    WALLS = (-1, 7)
    top_block: int = -1

    def __init__(self) -> None:
        self.grid = set()

    def drop(self, shape: Rock, movegen: Iterator[int]) -> None:
        position = (2, self.top_block + 4)
        while True:
            new_pos = position[0] + next(movegen), position[1]
            if not (
                shape.wall_collide(new_pos, self.WALLS)
                or shape.collide(new_pos, self.grid)
            ):
                position = new_pos

            new_pos = position[0], position[1] - 1
            if new_pos[1] < 0 or shape.collide(new_pos, self.grid):
                break
            else:
                position = new_pos
        self.top_block = max(
            max(p[1] + position[1] for p in shape.positions), self.top_block
        )
        self.grid.update(vadd(p, position) for p in shape.positions)

    def print_grid(self) -> None:
        for y in range(self.top_block + 3, -1, -1):
            s = "|"
            for x in range(self.WALLS[0] + 1, self.WALLS[1]):
                s += "#" if (x, y) in self.grid else "."
            s += "|"
            print(s)
        print("+" + "-" * (self.WALLS[1] - self.WALLS[0] - 1) + "+")


if __name__ == "__main__":
    PUZZLE_INPUT = input("> ")
    MOVES = cycle(-1 if c == "<" else 1 for c in PUZZLE_INPUT)
    SHAPES = cycle(ROCKS)

    gfbs = GenericFallingBlockSimulator()

    ITERATIONS = 2022
    for _, shape in zip(range(ITERATIONS), SHAPES):
        gfbs.drop(shape, MOVES)
        # gfbs.print_grid()

    print(f"Part 1: {gfbs.top_block + 1}")
