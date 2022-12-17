from typing import Iterable, Iterator, TypeVar


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


T_co = TypeVar("T_co", covariant=True)


class Cycle(Iterator[T_co]):
    _it: list[T_co]
    index: int

    def __init__(self, it: Iterable[T_co]) -> None:
        self._it = list(it)
        self.index = 0

    def __iter__(self) -> Iterator[T_co]:
        return self

    def __next__(self) -> T_co:
        i = self._it[self.index]
        self.index = (self.index + 1) % len(self._it)
        return i


if __name__ == "__main__":
    PUZZLE_INPUT = input("> ")
    MOVES = Cycle(-1 if c == "<" else 1 for c in PUZZLE_INPUT)
    SHAPES = Cycle(ROCKS)

    for NAME, ITERATIONS in (("Part 1", 2022), ("Part 2", 1_000_000_000_000)):
        gfbs = GenericFallingBlockSimulator()
        MOVES.index = 0
        SHAPES.index = 0

        i = 0
        p2_top_boost = 0
        seen_cyclepos: dict[tuple[int, int], tuple[int, int]] = dict()
        skip_target = None
        while i < ITERATIONS:
            shape = next(SHAPES)

            gfbs.drop(shape, MOVES)
            # gfbs.print_grid()

            if (SHAPES.index, MOVES.index) in seen_cyclepos:
                if skip_target is None:
                    s1 = seen_cyclepos[SHAPES.index, MOVES.index]
                    s2 = i, gfbs.top_block
                    di, dh = s2[0] - s1[0], s2[1] - s1[1]
                    skip_target = i + di
                elif i == skip_target:
                    s1 = seen_cyclepos[SHAPES.index, MOVES.index]
                    s2 = i, gfbs.top_block
                    di, dh = s2[0] - s1[0], s2[1] - s1[1]
                    skip_loops = (ITERATIONS - i) // di
                    if skip_loops:
                        i += skip_loops * di
                        p2_top_boost += skip_loops * dh
                        print("WARP SPEED", skip_loops)
            else:
                seen_cyclepos[SHAPES.index, MOVES.index] = i, gfbs.top_block

            i += 1
        print(f"{NAME}: {gfbs.top_block + 1 + p2_top_boost}")
