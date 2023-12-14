from dataclasses import dataclass

from puzzle_input import puzzle_input


@dataclass
class RockMap:
    dim: tuple[int, int]
    round_rocks: set[tuple[int, int]]
    cube_rocks: set[tuple[int, int]]

    @classmethod
    def from_map(cls, map_: list[str]):
        roundr: set[tuple[int, int]] = set()
        cuber: set[tuple[int, int]] = set()
        for y, line in enumerate(map_):
            for x, c in enumerate(line):
                if c == "O":
                    roundr.add((x, y))
                elif c == "#":
                    cuber.add((x, y))
                else:
                    assert c == "."
        return cls(dim=(len(map_[0]), len(map_)), round_rocks=roundr, cube_rocks=cuber)

    def tilt_up(self) -> None:
        for x in range(self.dim[0]):
            by = 0  # border - next position for rolling rock
            for cy in range(self.dim[1]):
                p = (x, cy)
                if p in self.round_rocks:
                    if p != (x, by):
                        self.round_rocks.remove(p)
                        self.round_rocks.add((x, by))
                    by += 1
                elif p in self.cube_rocks:
                    by = cy + 1

    def get_total_load(self) -> int:
        return sum(self.dim[1] - y for _, y in self.round_rocks)

    def print(self) -> None:
        lines: list[str] = []
        for y in range(self.dim[1]):
            line = ""
            for x in range(self.dim[0]):
                p = (x, y)
                assert not (p in self.round_rocks and p in self.cube_rocks)
                if p in self.round_rocks:
                    line += "O"
                elif p in self.cube_rocks:
                    line += "#"
                else:
                    line += "."
            lines.append(line)
        print("\n".join(lines))


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    rocks = RockMap.from_map(PUZZLE_INPUT)
    rocks.tilt_up()

    print("Part 1:", rocks.get_total_load())
