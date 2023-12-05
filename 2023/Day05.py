from dataclasses import dataclass
from typing import Collection, Iterable

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

    def map(self, value: int) -> int | None:
        if self.src <= value < self.src + self.range:
            return value - self.src + self.dest
        return None

    def map_r(self, value: Range2) -> tuple[Collection[Range2], Collection[Range2]]:
        left = value.start
        right = value.start + value.length

        if self.src + self.range <= left:
            return ((), (value,))  # unaffected
        elif right <= self.src:
            return ((), (value,))  # unaffected

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
        return (middle_range,), outer_ranges


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
            if new is not None:
                return new
        return value

    def map_r(self, values: Iterable[Range2]) -> Iterable[Range2]:
        for m in self.maps:
            new_values: list[Range2] = []
            for r in values:
                middle, sides = m.map_r(r)
                yield from middle
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
    seeds_i = list(map(int, seeds.split()))

    stat_i = seeds_i
    for m in mappings:
        stat_i = map(m.map, stat_i)
        # stat_i = list(stat_i)
        # print(stat_i)
    locs = stat_i
    print("Part 1:", min(locs))

    stat_i = Range2.from_seeds(seeds_i)
    # print(stat_i)
    for m in mappings:
        stat_i = m.map_r(stat_i)
        # print(stat_i)
    locs = stat_i
    print("Part 2:", min(r.start for r in locs))
