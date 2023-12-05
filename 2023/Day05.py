from dataclasses import dataclass
from typing import Iterable

from puzzle_input import puzzle_input


@dataclass
class Range2:
    start: int
    length: int

    @classmethod
    def from_seeds(cls, nums: Iterable[int]) -> Iterable["Range2"]:
        nums = list(nums)
        return (Range2(*pair) for pair in zip(nums[::2], nums[1::2]))


@dataclass
class MapLine:
    dest: int
    src: int
    range: int

    @classmethod
    def from_line(cls, line: str):
        return cls(*map(int, line.split()))

    def map(self, value: int) -> int:
        if self.src <= value < self.src + self.range:
            value += -self.src + self.dest
        return value

    def map_r(self, value: Range2) -> tuple[Range2 | None, list[Range2]]:
        if self.src + self.range <= value.start:
            return (None, [value])
        elif value.start + value.length <= self.src:
            return (None, [value])

        left = value.start
        right = value.start + value.length
        outer_ranges: list[Range2] = []
        if left < self.src:
            new_left = self.src
            outer_ranges.append(Range2(left, new_left - left))
            left = new_left
        if right > self.src + self.range:
            new_right = self.src + self.range
            outer_ranges.append(Range2(new_right, right - new_right))
            right = new_right
        middle_range = Range2(left - self.src + self.dest, right - left)
        return middle_range, outer_ranges


@dataclass
class Mapping:
    src_t: str
    dest_t: str
    maps: list[MapLine]

    @classmethod
    def from_block(cls, block: str):
        head, *lines = block.splitlines()
        assert head.endswith(" map:"), head
        head = head[: -len(" map:")]
        src_t, _, dest_t = head.split("-")
        return cls(src_t=src_t, dest_t=dest_t, maps=list(map(MapLine.from_line, lines)))

    def map(self, value: int) -> int:
        for m in self.maps:
            new = m.map(value)
            if new != value:
                return new
        return value

    def map_r(self, values: list[Range2]) -> Iterable[Range2]:
        values = values.copy()
        for m in self.maps:
            new_values: list[Range2] = []
            while values:
                middle, sides = m.map_r(values.pop())
                if middle is not None:
                    yield middle
                new_values.extend(sides)
            values = new_values
            if not values:
                return  # mapped everything
        yield from values  # unchanged


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().split("\n\n")
    seeds = PUZZLE_INPUT[0]
    mappings = list(map(Mapping.from_block, PUZZLE_INPUT[1:]))

    assert seeds.startswith("seeds: "), seeds
    seeds = seeds[len("seeds: ") :]

    stat_i = map(int, seeds.split())
    for m in mappings:
        stat_i = map(m.map, stat_i)
        # stat_i = list(stat_i)
        # print(stat_i)
    locs = list(stat_i)
    print("Part 1:", min(locs))

    stat_i = list(Range2.from_seeds(map(int, seeds.split())))
    # print(stat_i)
    for m in mappings:
        stat_i = list(m.map_r(stat_i))
        # print(stat_i)
    locs = stat_i
    print("Part 2:", min(r.start for r in locs))
