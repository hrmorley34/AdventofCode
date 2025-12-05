from dataclasses import dataclass

from puzzle_input import puzzle_input


@dataclass(order=True)
class Range:
    start: int
    end: int

    @classmethod
    def from_str(cls, s: str):
        start, end = s.split("-")
        return cls(start=int(start), end=int(end))

    def __contains__(self, item: int) -> bool:
        return self.start <= item <= self.end

    def merge(self, neighbour: "Range") -> list["Range"]:
        if neighbour.start < self.start:
            return neighbour.merge(self)

        if neighbour.start <= self.end + 1:
            return [Range(self.start, max(self.end, neighbour.end))]
        else:
            return [self, neighbour]

    @staticmethod
    def mergeall(ranges: list["Range"]) -> list["Range"]:
        ranges.sort()
        while True:
            size = len(ranges)
            for index in range(len(ranges) - 2, -1, -1):
                ranges[index : index + 2] = ranges[index].merge(ranges[index + 1])
            if size == len(ranges):  # no more changes
                break
        return ranges


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    sranges, sitems = PUZZLE_INPUT.split("\n\n")
    ranges = list(map(Range.from_str, sranges.splitlines()))
    items = list(map(int, sitems.splitlines()))
    Range.mergeall(ranges)

    print("Part 1:", sum(any(item in ran for ran in ranges) for item in items))
    print("Part 2:", sum(ran.end - ran.start + 1 for ran in ranges))
