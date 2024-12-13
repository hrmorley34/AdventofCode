import re
from dataclasses import dataclass
from fractions import Fraction
from typing import Self

from puzzle_input import puzzle_input

CLAW_PATTERN = re.compile(
    r"Button A: X\+(\d+), Y\+(\d+)\n"
    r"Button B: X\+(\d+), Y\+(\d+)\n"
    r"Prize: X=(\d+), Y=(\d+)"
)

OFFSET2 = 10000000000000


@dataclass
class ClawMachine:
    a: tuple[int, int]
    b: tuple[int, int]
    prize: tuple[int, int]

    @classmethod
    def from_string(cls, s: str) -> Self:
        m = CLAW_PATTERN.fullmatch(s.strip())
        assert m is not None
        return cls(
            a=(int(m[1]), int(m[2])),
            b=(int(m[3]), int(m[4])),
            prize=(int(m[5]), int(m[6])),
        )

    @classmethod
    def from_string2(cls, s: str) -> Self:
        m = CLAW_PATTERN.fullmatch(s.strip())
        assert m is not None
        return cls(
            a=(int(m[1]), int(m[2])),
            b=(int(m[3]), int(m[4])),
            prize=(int(m[5]) + OFFSET2, int(m[6]) + OFFSET2),
        )

    def solve(self) -> tuple[int, int] | None:
        ax, ay = self.a
        bx, by = self.b
        tx, ty = self.prize
        b = (tx - Fraction(ty, ay) * ax) / (bx - Fraction(by, ay) * ax)
        a = Fraction(tx - b * bx, ax)
        if a.denominator != 1 or b.denominator != 1:
            # no integer solution
            return None
        a = a.numerator
        b = b.numerator
        assert a >= 0 and b >= 0
        return a, b

    def cost(self) -> int:
        s = self.solve()
        return 0 if s is None else (s[0] * 3 + s[1] * 1)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().split("\n\n")

    MACHINES = list(map(ClawMachine.from_string, PUZZLE_INPUT))
    print("Part 1:", sum(c.cost() for c in MACHINES))

    MACHINES2 = list(map(ClawMachine.from_string2, PUZZLE_INPUT))
    print("Part 2:", sum(c.cost() for c in MACHINES2))
