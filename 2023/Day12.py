from dataclasses import dataclass

from puzzle_input import puzzle_input


@dataclass
class SpringRow:
    row: str
    groups: tuple[int, ...]

    @classmethod
    def from_line(cls, line: str):
        row, groups = line.split()
        return cls(row=row, groups=tuple(map(int, groups.split(","))))

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
        if index + length > len(self.row):
            return None
        new_row = list(self.row)
        if replace_qs is not None:
            for i in range(replace_qs, index):
                if new_row[i] == "?":
                    new_row[i] = "."
        for offset in range(length):
            if new_row[index + offset] == "#":
                continue
            elif new_row[index + offset] == "?":
                new_row[index + offset] = "#"
            else:
                assert new_row[index + offset] == "."
                return None
        if index + length < len(new_row):  # ie !=
            if new_row[index + length] == "#":
                return None
            elif new_row[index + length] == "?":
                new_row[index + length] = "."
            assert new_row[index + length] == "."
        return SpringRow("".join(new_row), self.groups)

    def without_region(self, index: int, length: int) -> "SpringRow | None":
        if index + length > len(self.row):
            return None
        new_row = list(self.row)
        for offset in range(length):
            if new_row[index + offset] == "#":
                return None
            elif new_row[index + offset] == "?":
                new_row[index + offset] = "."
            assert new_row[index + offset] == "."
        return SpringRow("".join(new_row), self.groups)

    def get_possible(self, rindex: int = 0, gindex: int = 0) -> int:
        # print("GET_POSSIBLE", self, rindex, gindex)
        if gindex >= len(self.groups):
            r = self.without_region(rindex, len(self.row) - rindex)
            if r is not None and r.get_match():
                # print(self)
                return True
            return False
        target_length = self.groups[gindex]
        while rindex < len(self.row) and self.row[rindex] == ".":
            rindex += 1
        if rindex >= len(self.row):
            return 0
        elif self.row[rindex] == "#":
            r = self.with_region(rindex, target_length)
            if r is None:
                return 0
            return r.get_possible(rindex + target_length + 1, gindex + 1)

        assert self.row[rindex] == "?"
        replace_qs = rindex
        new_starts: list[int] = []
        while rindex < len(self.row) and self.row[rindex] != "#":
            if self.row[rindex] == "?":
                new_starts.append(rindex)
            rindex += 1
        if rindex >= len(self.row):
            pass
        else:
            assert self.row[rindex] == "#"
            new_starts.append(rindex)

        new_lines = [
            (ri, self.with_region(ri, target_length, replace_qs)) for ri in new_starts
        ]
        return sum(
            sr.get_possible(ri + target_length + 1, gindex + 1)
            for ri, sr in new_lines
            if sr is not None
        )


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    springs = list(map(SpringRow.from_line, PUZZLE_INPUT))

    print("Part 1:", sum(sr.get_possible() for sr in springs))
