from __future__ import annotations

from itertools import chain
from puzzle_input import puzzle_input
import re
from typing import Generator, Iterable, Literal, NamedTuple, Set


class Int3(NamedTuple):
    x: int
    y: int
    z: int

    def clamp(self, minv: Int3 | None, maxv: Int3 | None) -> Int3:
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

    def is_within(self, minv: Int3 | None, maxv: Int3 | None) -> bool:
        if minv is not None:
            if any(a < b for a, b in zip(self, minv)):
                return False
        if maxv is not None:
            if any(a > b for a, b in zip(self, maxv)):
                return False
        return True


COMMAND = re.compile(
    r"(?:(?P<on>on)|off) "
    r"x=(?P<minx>-?\d+)..(?P<maxx>-?\d+),"
    r"y=(?P<miny>-?\d+)..(?P<maxy>-?\d+),"
    r"z=(?P<minz>-?\d+)..(?P<maxz>-?\d+)"
)


def command_to_range(c: str) -> Range:
    m = COMMAND.fullmatch(c)
    assert m is not None
    return Range(
        m["on"] is not None,
        Int3(int(m["minx"]), int(m["miny"]), int(m["minz"])),
        Int3(int(m["maxx"]), int(m["maxy"]), int(m["maxz"])),
    )


def range3_inclusive(min: Int3, max: Int3) -> Generator[Int3, None, None]:
    for x in range(min.x, max.x + 1):
        for y in range(min.y, max.y + 1):
            for z in range(min.z, max.z + 1):
                yield Int3(x, y, z)


class Range(NamedTuple):
    active: bool
    min: Int3
    max: Int3

    def within(self, xyz: Int3) -> bool:
        return xyz.is_within(self.min, self.max)

    def overlap(self, range: Range) -> bool:
        return not (
            any(a < b for a, b in zip(self.max, range.min))
            or any(a > b for a, b in zip(self.min, range.max))
        )

    # def find_overlap(self, range: Range) -> Range:
    #     return Range(
    #         Int3(*(max(a, b) for a, b in zip(self.min, range.min))),
    #         Int3(*(min(a, b) for a, b in zip(self.max, range.max))),
    #     )

    def contains(self, range: Range) -> bool:
        return all(a >= b for a, b in zip(self.max, range.max)) and all(
            a <= b for a, b in zip(self.min, range.min)
        )

    def has_volume(self) -> bool:
        return all(a <= b for a, b in zip(self.min, self.max))

    def volume(self) -> int:
        return (
            (self.max.x + 1 - self.min.x)
            * (self.max.y + 1 - self.min.y)
            * (self.max.z + 1 - self.min.z)
        )

    def setrange(self, range: Range) -> RangeSet:
        assert self.active
        if not self.overlap(range):
            if not range.active:
                return RangeSet({self})
            return RangeSet({self, range})
        elif self.contains(range) and range.active:
            return RangeSet({self})
        # elif self.contains(range):
        #     return self._xor_contains(range)
        # elif range.contains(self):
        #     return range._xor_contains(self)
        # else:
        #     return self._xor_overlap(range)

        splits = tuple(
            {selfmin, selfmax + 1, rangemin, rangemax + 1}
            for selfmin, selfmax, rangemin, rangemax in zip(
                self.min, self.max, range.min, range.max
            )
        )
        sself = self.split(*splits)
        srange = range.split(*splits)
        if range.active:
            return RangeSet({self} | (srange - sself))
        else:
            return RangeSet(sself - srange)

    def split(
        self, x: Iterable[int] = (), y: Iterable[int] = (), z: Iterable[int] = ()
    ) -> RangeSet:
        r: set[Range] = {self}
        xs = set(x)
        if xs:
            r = set(self._split1(r, "x", xs))
        ys = set(y)
        if ys:
            r = set(self._split1(r, "y", ys))
        zs = set(z)
        if zs:
            r = set(self._split1(r, "z", zs))
        return RangeSet(r)

    @staticmethod
    def _split1(
        ranges: set[Range], dir: Literal["x", "y", "z"], points: set[int]
    ) -> Generator[Range, None, None]:
        lastsplit: int | None = None
        for split in chain(points, (None,)):
            replacemin = {} if lastsplit is None else {dir: lastsplit}
            replacemax = {} if split is None else {dir: split - 1}
            for r in ranges:
                newr = Range(
                    r.active, r.min._replace(**replacemin), r.max._replace(**replacemax)
                )
                if newr.has_volume():
                    yield newr
            lastsplit = split

    # def _xor_contains(self, range: Range) -> set[Range]:
    #     xsplits: set[int] = set()
    #     ysplits: set[int] = set()
    #     zsplits: set[int] = set()

    #     for selfmin, selfmax, rangemin, rangemax, splits in zip(
    #         self.min, self.max, range.min, range.max, (xsplits, ysplits, zsplits)
    #     ):
    #         if selfmin != rangemin:
    #             splits.add(rangemin)
    #         if selfmax != rangemax:
    #             splits.add(rangemax + 1)

    #     s = self.split(xsplits, ysplits, zsplits)
    #     s.remove(range)
    #     return s

    # def _xor_overlap(self, range: Range) -> set[Range]:
    #     xsplits: set[int] = set()
    #     ysplits: set[int] = set()
    #     zsplits: set[int] = set()

    #     for selfmin, selfmax, rangemin, rangemax, splits in zip(
    #         self.min, self.max, range.min, range.max, (xsplits, ysplits, zsplits)
    #     ):
    #         splits.update((selfmin, selfmax + 1, rangemin, rangemax + 1))
    #     return self.split(xsplits, ysplits, zsplits) ^ range.split(xsplits, ysplits, zsplits)


class RangeSet(Set[Range]):
    active = True

    def within(self, xyz: Int3) -> bool:
        return any(r.within(xyz) for r in self)

    def overlap(self, range: Range) -> bool:
        return any(r.overlap(range) for r in self)

    def has_volume(self) -> bool:
        return len(self) > 0

    def volume(self) -> int:
        return sum(r.volume() for r in self)

    def setrange(self, range: Range) -> RangeSet:
        if not self.overlap(range):
            if range.active:
                return RangeSet({*self, range})
            return self

        incl: set[Range] = set()
        excl: set[Range] = set()
        for r in self:
            if r.overlap(range):
                incl.add(r)
            else:
                excl.add(r)

        splits = tuple(
            set(chain(*({r.min[index], r.max[index] + 1} for r in incl)))
            for index in (0, 1, 2)
        )

        sself = set(chain(*(r.split(*splits) for r in incl)))
        srange = range.split(*splits)
        if range.active:
            return RangeSet(self | (srange - sself))
        else:
            return RangeSet((sself - srange) | excl)


class Grid:
    on: set[Int3]
    hard_range: tuple[Int3 | None, Int3 | None]

    def __init__(self, minv: Int3 | None, maxv: Int3 | None) -> None:
        self.on = set()
        self.hard_range = (minv, maxv)

    def count_on(self) -> int:
        return len(self.on)

    def set_range(self, state: bool, minv: Int3, maxv: Int3):
        operation = self.on.update if state else self.on.difference_update
        if self.hard_range[0] is not None or self.hard_range[1] is not None:
            minv, maxv = minv.clamp(self.hard_range[0], None), maxv.clamp(
                None, self.hard_range[1]
            )
            if minv != minv.clamp(*self.hard_range) or maxv != maxv.clamp(
                *self.hard_range
            ):
                return
        operation(range3_inclusive(minv, maxv))

    def run_command(self, c: str):
        self.set_range(*command_to_range(c))


class RangeGrid:
    on: RangeSet

    def __init__(self) -> None:
        self.on = RangeSet()

    def count_on(self) -> int:
        return self.on.volume()

    def run_command(self, c: str):
        range = command_to_range(c)
        self.on = self.on.setrange(range)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    grid = Grid(Int3(-50, -50, -50), Int3(50, 50, 50))
    for c in PUZZLE_INPUT:
        grid.run_command(c)
    print("Part 1:", grid.count_on())

    # SLOW AND BROKEN
    # grid2 = RangeGrid()
    # COUNT = len(PUZZLE_INPUT)
    # for i, c in enumerate(PUZZLE_INPUT):
    #     print(f"{i:3}/{COUNT:3}", ":", c, ":", len(grid2.on))
    #     grid2.run_command(c)
    # print("Part 2:", grid2.count_on())
