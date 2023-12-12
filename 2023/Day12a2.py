from dataclasses import dataclass
from functools import lru_cache

from puzzle_input import puzzle_input


@dataclass(frozen=True)
class SpringRow:
    row: str
    groups: tuple[int, ...]

    @classmethod
    def from_line(cls, line: str):
        row, groups = line.split()
        return cls(row=row, groups=tuple(map(int, groups.split(","))))

    @classmethod
    def from_line_expand(cls, line: str):
        row, groups = line.split()
        return cls(
            row="?".join([row] * 5), groups=tuple(map(int, groups.split(","))) * 5
        )

    def get_resolved(self) -> bool:
        return "?" not in self.row

    def get_match(self) -> bool:
        return (
            self.get_resolved()
            and tuple(map(len, self.row.replace(".", " ").split())) == self.groups
        )

    def with_region(
        self, index: int, length: int, replace_qs: int | None = None
    ) -> "SpringRow | None":
        assert length == self.groups[0]

        if index + length > len(self.row):
            return None

        if replace_qs is not None:
            for i in range(replace_qs, index):
                if self.row[i] == "#":
                    return None
        for offset in range(length):
            if self.row[index + offset] == ".":
                return None
        if index + length < len(self.row):  # ie !=
            if self.row[index + length] == "#":
                return None
        return SpringRow(self.row[index + length + 1 :], self.groups[1:])

    def without_region(self, index: int, length: int) -> "SpringRow | None":
        if index + length > len(self.row):
            return None
        for offset in range(length):
            if self.row[index + offset] == "#":
                return None
        return SpringRow(self.row[index + length :], self.groups)

    @lru_cache
    def get_possible(self) -> int:
        # print("GET_POSSIBLE", self, rindex, gindex)
        if not len(self.groups):
            return "#" not in self.row
        if not len(self.row):
            return 0

        rindex = 0
        if self.row[0] == ".":
            rindex = 0
            while rindex < len(self.row) and self.row[rindex] == ".":
                rindex += 1
            if rindex >= len(self.row):
                return 0
            return SpringRow(self.row[rindex:], self.groups).get_possible()

        target_length = self.groups[0]
        if self.row[rindex] == "#":
            r = self.with_region(rindex, target_length)
            if r is None:
                return 0
            return r.get_possible()

        assert self.row[rindex] == "?"
        replace_qs = rindex
        new_starts: list[int] = []
        max_rindex = len(self.row) - sum(1 + i for i in self.groups[1:])
        while rindex < max_rindex and self.row[rindex] != "#":
            if self.row[rindex] == "?":
                new_starts.append(rindex)
            rindex += 1
        if rindex >= max_rindex:
            pass
        else:
            assert self.row[rindex] == "#"
            new_starts.append(rindex)

        new_lines = [
            self.with_region(ri, target_length, replace_qs) for ri in new_starts
        ]
        return sum(sr.get_possible() for sr in new_lines if sr is not None)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    springs = list(map(SpringRow.from_line, PUZZLE_INPUT))
    print("Part 1:", sum(sr.get_possible() for sr in springs))

    springs2 = list(map(SpringRow.from_line_expand, PUZZLE_INPUT))
    print("Part 2:", sum(sr.get_possible() for sr in springs2))
