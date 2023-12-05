from dataclasses import dataclass

from puzzle_input import puzzle_input


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
