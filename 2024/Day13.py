import re
from dataclasses import dataclass
from typing import Self

from puzzle_input import puzzle_input

CLAW_PATTERN = re.compile(
    r"Button A: X\+(\d+), Y\+(\d+)\n"
    r"Button B: X\+(\d+), Y\+(\d+)\n"
    r"Prize: X=(\d+), Y=(\d+)"
)


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

    def solve(self) -> tuple[int, int] | None:
        a = min(self.prize[1] // self.a[1], 100)
        b = (self.prize[1] - a * self.a[1]) // self.b[1]
        while (
            self.a[0] * a + self.b[0] * b != self.prize[0]
            or self.a[1] * a + self.b[1] * b != self.prize[1]
        ):
            a -= 1
            b = (self.prize[1] - a * self.a[1]) // self.b[1]
            if a < 0 or b > 100:
                return None
        return a, b

    def cost(self) -> int:
        s = self.solve()
        return 0 if s is None else (s[0] * 3 + s[1] * 1)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().split("\n\n")
    MACHINES = list(map(ClawMachine.from_string, PUZZLE_INPUT))

    print("Part 1:", sum(c.cost() for c in MACHINES))
