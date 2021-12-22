from puzzle_input import puzzle_input
import re
from typing import Generator, NamedTuple


class Int3(NamedTuple):
    x: int
    y: int
    z: int

    def clamp(self, minv: "Int3 | None", maxv: "Int3 | None") -> "Int3":
        x, y, z = self
        if maxv is not None:
            x = min(x, maxv.x)
            y = min(y, maxv.y)
            z = min(z, maxv.z)
        if minv is not None:
            x = max(x, minv.x)
            y = max(y, minv.y)
            z = max(z, minv.z)
        return Int3(x, y, z)


COMMAND = re.compile(
    r"(?:(?P<on>on)|off) "
    r"x=(?P<minx>-?\d+)..(?P<maxx>-?\d+),"
    r"y=(?P<miny>-?\d+)..(?P<maxy>-?\d+),"
    r"z=(?P<minz>-?\d+)..(?P<maxz>-?\d+)"
)


def command_to_range(c: str) -> tuple[bool, Int3, Int3]:
    m = COMMAND.fullmatch(c)
    assert m is not None
    return (
        m["on"] is not None,
        Int3(int(m["minx"]), int(m["miny"]), int(m["minz"])),
        Int3(int(m["maxx"]), int(m["maxy"]), int(m["maxz"])),
    )


def range3_inclusive(min: Int3, max: Int3) -> Generator[Int3, None, None]:
    for x in range(min.x, max.x + 1):
        for y in range(min.y, max.y + 1):
            for z in range(min.z, max.z + 1):
                yield Int3(x, y, z)


class Grid:
    on: set[Int3]
    hard_range: tuple[Int3, Int3]

    def __init__(self, minv: Int3, maxv: Int3) -> None:
        self.on = set()
        self.hard_range = (minv, maxv)

    def count_on(self) -> int:
        return len(self.on)

    def set_range(self, state: bool, minv: Int3, maxv: Int3):
        operation = self.on.add if state else self.on.discard
        minv, maxv = minv.clamp(self.hard_range[0], None), maxv.clamp(
            None, self.hard_range[1]
        )
        if minv != minv.clamp(*self.hard_range) or maxv != maxv.clamp(*self.hard_range):
            return
        for i3 in range3_inclusive(minv, maxv):
            operation(i3)

    def run_command(self, c: str):
        self.set_range(*command_to_range(c))


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    grid = Grid(Int3(-50, -50, -50), Int3(50, 50, 50))
    for c in PUZZLE_INPUT:
        grid.run_command(c)
    print("Part 1:", grid.count_on())
