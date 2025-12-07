from collections import Counter

from puzzle_input import puzzle_input


class Grid:
    grid: list[str]
    splitters: list[set[int]]

    def __init__(self, grid: list[str]) -> None:
        self.grid = grid
        self.splitters = [
            {i for i, c in enumerate(line) if c == "^"} for line in self.grid
        ]

    @staticmethod
    def split_on(x: set[int], y: set[int]) -> set[int]:
        return (x - y) | set[int]().union(*((i - 1, i + 1) for i in (x & y)))

    def run_split(self) -> tuple[int, int]:
        split_count = 0
        timelines = Counter([self.grid[0].index("S")])
        for rowi in range(1, len(self.grid)):
            intersection = set(timelines) & self.splitters[rowi]
            split_count += len(intersection)
            for split in intersection:
                count = timelines.pop(split)
                timelines[split - 1] += count
                timelines[split + 1] += count
        return split_count, sum(timelines.values())


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    grid = Grid(PUZZLE_INPUT.splitlines())

    p1, p2 = grid.run_split()
    print("Part 1:", p1)
    print("Part 2:", p2)
