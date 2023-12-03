from string import digits

from puzzle_input import puzzle_input


class Engine:
    parts: dict[tuple[int, int], set[int]]
    symbols: set[str]
    stranded: set[int]

    def __init__(self) -> None:
        self.parts = {}
        self.symbols = set()
        self.stranded = set()

    @classmethod
    def from_grid(cls, grid: list[str]):
        self = cls()
        for y, line in enumerate(grid):
            for x, c in enumerate(line):
                if c != "." and c not in digits:
                    self.symbols.add(c)
                    self.parts[(x, y)] = set()
        for y, line in enumerate(grid):
            cur_num = ""
            cur_pos: tuple[int, int] | None = None
            for x, c in enumerate(line):
                if c in digits:
                    if not cur_num and x > 0:
                        for py in range(max(y - 1, 0), min(len(grid), y + 2)):
                            if (x - 1, py) in self.parts:
                                assert cur_pos is None, (
                                    "Num can only tie to one part",
                                    (x - 1, py),
                                    cur_num,
                                )
                                cur_pos = (x - 1, py)
                    for py in range(max(y - 1, 0), min(len(grid), y + 2)):
                        if py == 0:
                            continue
                        if (x, py) in self.parts:
                            assert cur_pos is None, (
                                "Num can only tie to one part",
                                (x, py),
                                cur_num,
                            )
                            cur_pos = (x, py)
                    cur_num += c
                elif cur_num:
                    for py in range(max(y - 1, 0), min(len(grid), y + 2)):
                        if (x, py) in self.parts:
                            assert cur_pos is None, (
                                "Num can only tie to one part",
                                (x + 1, py),
                                cur_num,
                            )
                            cur_pos = (x, py)
                    if cur_pos is not None:
                        self.parts[cur_pos].add(int(cur_num))
                    else:
                        self.stranded.add(int(cur_num))
                    cur_num = ""
                    cur_pos = None
            if cur_num:
                if cur_pos is not None:
                    self.parts[cur_pos].add(int(cur_num))
                else:
                    self.stranded.add(int(cur_num))
        return self

    def part1(self) -> int:
        return sum(map(sum, self.parts.values()))

    def part2(self) -> int:
        return sum(a * b for a, b in filter(lambda s: len(s) == 2, self.parts.values()))


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    e = Engine.from_grid(PUZZLE_INPUT)
    print("Part 1:", e.part1())
    print("Part 2:", e.part2())
