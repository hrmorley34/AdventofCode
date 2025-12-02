from dataclasses import dataclass
from typing import Iterable


def alleq[T](items: Iterable[T]) -> bool:
    it = iter(items)
    cmp = next(it)
    return all(cmp == item for item in it)


def is_invalid(i: int, two_only: bool) -> bool:
    si = str(i)
    digit_count = len(si)
    if digit_count < 2:
        return False
    for factor in [digit_count // 2] if two_only else range(1, digit_count // 2 + 1):
        if digit_count % factor != 0:
            continue
        if alleq(si[i : i + factor] for i in range(0, len(si), factor)):
            return True
    return False


@dataclass
class Range:
    min: int
    max: int

    @classmethod
    def from_string(cls, s: str):
        min, max = s.split("-")
        return cls(min=int(min), max=int(max))

    def sum_invalid(self, two_only: bool) -> int:
        return sum(
            i for i in range(self.min, self.max + 1) if is_invalid(i, two_only=two_only)
        )


if __name__ == "__main__":
    PUZZLE_INPUT = input("> ")
    ranges = list(map(Range.from_string, PUZZLE_INPUT.split(",")))
    print("Part 1:", sum(r.sum_invalid(True) for r in ranges))
    print("Part 2:", sum(r.sum_invalid(False) for r in ranges))
