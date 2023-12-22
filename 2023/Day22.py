from __future__ import annotations

from functools import lru_cache
from typing import Iterable, NamedTuple

from puzzle_input import puzzle_input


class Vec3(NamedTuple):
    x: int
    y: int
    z: int

    def __repr__(self) -> str:
        return f"<{self.x}, {self.y}, {self.z}>"


class Brick:
    start: Vec3
    end: Vec3
    supporting: list[Brick]
    supported_by: list[Brick]

    def __init__(
        self,
        start: Vec3 | tuple[int, int, int],
        end: Vec3 | tuple[int, int, int],
        supports: Iterable[Brick],
    ) -> None:
        self.start = Vec3(*start)
        self.end = Vec3(*end)
        self.supporting = []
        self.supported_by = list(supports)
        for support in self.supported_by:
            support.supporting.append(self)

    def __repr__(self) -> str:
        return f"<Brick {self.start}, {self.end}, {len(self.supported_by)} supports>"

    def __hash__(self) -> int:
        return hash((self.start, self.end))

    @classmethod
    def from_line(cls, line: str) -> Brick:
        s, e = line.split("~")
        (sx, sy, sz), (ex, ey, ez) = map(int, s.split(",")), map(int, e.split(","))
        return cls((sx, sy, sz), (ex, ey, ez), [])

    @property
    def min_z(self) -> int:
        return min(self.start.z, self.end.z)

    @property
    def max_z(self) -> int:
        return max(self.start.z, self.end.z)

    @staticmethod
    def covers_1d(
        below_start: int, below_end: int, above_start: int, above_end: int
    ) -> bool:
        below_start, below_end = sorted((below_start, below_end))
        above_start, above_end = sorted((above_start, above_end))
        return (
            below_start <= above_start <= below_end
            or below_start <= above_end <= below_end
            or (above_start <= below_start and below_end <= above_end)
            or (above_start <= below_start and below_end <= above_end)
        )

    def is_below(self, brick: Brick) -> bool:
        return (
            self.covers_1d(self.start.x, self.end.x, brick.start.x, brick.end.x)
            and self.covers_1d(self.start.y, self.end.y, brick.start.y, brick.end.y)
            and self.max_z < brick.min_z
        )

    def fall(self, below: Iterable[Brick]) -> Brick:
        below = [b for b in below if b.is_below(self)]
        max_z = max((b.max_z for b in below), default=0)
        dz = -self.min_z + max_z + 1
        return Brick(
            (self.start.x, self.start.y, self.start.z + dz),
            (self.end.x, self.end.y, self.end.z + dz),
            [b for b in below if b.max_z == max_z],
        )

    def disintegratable(self) -> bool:
        return all(len(b.supported_by) > 1 for b in self.supporting)

    @lru_cache
    def get_above_r(self) -> set[Brick]:
        return set(self.supporting).union(*(b.get_above_r() for b in self.supporting))

    def count_disintegrates(self) -> int:
        above = list(self.get_above_r())
        above.sort(key=lambda b: b.min_z)

        disint: set[Brick] = {self}
        for b in above:
            if set(b.supported_by) - disint == set():
                disint.add(b)
        return len(disint) - 1  # don't count self


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    bricks = list(map(Brick.from_line, PUZZLE_INPUT))
    bricks.sort(key=lambda b: b.min_z)

    new_bricks: list[Brick] = []
    for brick in bricks:
        new_bricks.append(brick.fall(new_bricks))
    print("Part 1:", sum(b.disintegratable() for b in new_bricks))

    print("Part 2:", sum(b.count_disintegrates() for b in new_bricks))
