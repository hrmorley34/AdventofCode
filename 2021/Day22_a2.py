from __future__ import annotations

from itertools import chain
from puzzle_input import puzzle_input
import re
from typing import Generator, Iterable, Literal, NamedTuple


class Int3(NamedTuple):
    x: int
    y: int
    z: int


COMMAND = re.compile(
    r"(?:(?P<on>on)|off) "
    r"x=(?P<minx>-?\d+)..(?P<maxx>-?\d+),"
    r"y=(?P<miny>-?\d+)..(?P<maxy>-?\d+),"
    r"z=(?P<minz>-?\d+)..(?P<maxz>-?\d+)"
)


def command_to_range(c: str) -> ActiveRange:
    m = COMMAND.fullmatch(c)
    assert m is not None
    return ActiveRange(
        m["on"] is not None,
        Int3(int(m["minx"]), int(m["miny"]), int(m["minz"])),
        Int3(int(m["maxx"]), int(m["maxy"]), int(m["maxz"])),
    )


class Range(NamedTuple):
    min: Int3
    max: Int3

    def overlap(self, range: Range) -> bool:
        return not (
            any(a < b for a, b in zip(self.max, range.min))
            or any(a > b for a, b in zip(self.min, range.max))
        )

    def contains(self, range: Range) -> bool:
        return all(a >= b for a, b in zip(self.max, range.max)) and all(
            a <= b for a, b in zip(self.min, range.min)
        )

    def has_volume(self) -> bool:
        return all(a <= b for a, b in zip(self.min, self.max))

    def volume(self) -> int:
        assert self.has_volume()
        return (
            (self.max.x + 1 - self.min.x)
            * (self.max.y + 1 - self.min.y)
            * (self.max.z + 1 - self.min.z)
        )

    def remove(self, range: Range) -> set[Range]:
        if not self.overlap(range):
            return {self}

        splits = tuple(
            {selfmin, selfmax + 1, rangemin, rangemax + 1}
            for selfmin, selfmax, rangemin, rangemax in zip(
                self.min, self.max, range.min, range.max
            )
        )
        sself = self.split(*splits)
        srange = range.split(*splits)
        return sself - srange

    def split(
        self, x: Iterable[int] = (), y: Iterable[int] = (), z: Iterable[int] = ()
    ) -> set[Range]:
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
        return r

    @staticmethod
    def _split1(
        ranges: set[Range], dir: Literal["x", "y", "z"], points: set[int]
    ) -> Generator[Range, None, None]:
        lastsplit: int | None = None
        for split in chain(sorted(points), (None,)):
            replacemin = {} if lastsplit is None else {dir: lastsplit}
            replacemax = {} if split is None else {dir: split - 1}
            for r in ranges:
                newr = Range(r.min._replace(**replacemin), r.max._replace(**replacemax))
                if newr.has_volume() and r.contains(newr):
                    yield newr
            lastsplit = split


class ActiveRange(NamedTuple):
    active: bool
    min: Int3
    max: Int3

    def to_range(self) -> Range:
        return Range(self.min, self.max)


def rangeset_size(actual: ActiveRange, ranges: set[Range]) -> int:
    if not actual.active:
        return 0

    s = {actual.to_range()}
    for r in ranges:
        s = set(chain(*(a.remove(r) for a in s)))
        if not s:
            return 0
    return sum(a.volume() for a in s)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    COUNT = len(PUZZLE_INPUT)
    total = 0
    ranges: set[Range] = set()
    for i, c in enumerate(reversed(PUZZLE_INPUT), 1):
        r = command_to_range(c)
        total += rangeset_size(r, ranges)
        print(f"{i:3}/{COUNT:3}", ":", c, ":", total)
        ranges.add(r.to_range())
    print("Part 2:", total)
