from dataclasses import dataclass

from puzzle_input import puzzle_input


def rotate(t: tuple[int, int], d: int, dim: tuple[int, int]) -> tuple[int, int]:
    (a, b) = t
    maxx, maxy = dim
    if d % 4 == 1:
        return b, maxy - a - 1
    if d % 4 == 2:
        return maxx - a - 1, maxy - b - 1
    if d % 4 == 3:
        return maxx - b - 1, a
    else:
        return a, b


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

    def tilt_dir(self, dir: int = 0) -> None:
        "dir: 0 up, 1 left, 2 down, 3 right"
        for x in range(self.dim[dir % 2]):
            by = 0  # border - next position for rolling rock
            for cy in range(self.dim[(dir + 1) % 2]):
                p = rotate((x, cy), dir, self.dim)
                newp = rotate((x, by), dir, self.dim)
                if p in self.round_rocks:
                    if p != newp:
                        self.round_rocks.remove(p)
                        self.round_rocks.add(newp)
                    by += 1
                elif p in self.cube_rocks:
                    by = cy + 1

    def spin_cycle(self) -> None:
        for i in range(4):
            self.tilt_dir(i)

    def spin_n(self, n: int) -> None:
        seen: dict[frozenset[tuple[int, int]], int] = {frozenset(self.round_rocks): 0}

        i = 0
        while i < n:
            self.spin_cycle()
            i += 1

            roundset = frozenset(self.round_rocks)
            if roundset in seen:
                jump = i - seen[roundset]
                while i < n - jump + 1:
                    i += jump
            else:
                seen[roundset] = i

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

    rocks.tilt_dir(0)
    print("Part 1:", rocks.get_total_load())

    rocks.spin_n(1000000000)
    print("Part 2:", rocks.get_total_load())
