import operator as op
from collections.abc import Callable, Generator
from dataclasses import dataclass

from puzzle_input import puzzle_input

Operator = tuple[str, Callable[[int, int], int]]
OPERATORS1: list[Operator] = [("+", op.add), ("*", op.mul)]
OPERATORS2: list[Operator] = OPERATORS1 + [("||", lambda a, b: int(str(a) + str(b)))]


@dataclass
class CalibrationEquation:
    result: int
    operands: list[int]

    @classmethod
    def from_string(cls, line: str) -> "CalibrationEquation":
        r, o = line.split(": ", 1)
        return cls(int(r), list(map(int, o.split())))

    def is_valid(self, operators: list[Operator]) -> bool:
        try:
            next(self.get_operators(operators))
        except StopIteration:
            return False
        else:
            return True

    def evaluate(self, operators: tuple[Operator, ...]) -> int:
        result, *ix = self.operands
        for o, i in zip(operators, ix):
            result = o[1](result, i)
        return result

    def get_operators(
        self, operators: list[Operator], prefix: tuple[Operator, ...] = ()
    ) -> Generator[tuple[Operator, ...], None, None]:
        assert len(prefix) < len(self.operands)
        if len(prefix) >= len(self.operands) - 1:
            if self.evaluate(prefix) == self.result:
                yield prefix
            return
        if self.evaluate(prefix) > self.result:
            # none of the operators can shrink a number, so we just give up on this path
            return
        for o in operators:
            yield from self.get_operators(operators=operators, prefix=prefix + (o,))


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    EQUATIONS = list(map(CalibrationEquation.from_string, PUZZLE_INPUT))

    print("Part 1:", sum(eq.result for eq in EQUATIONS if eq.is_valid(OPERATORS1)))
    print("Part 2:", sum(eq.result for eq in EQUATIONS if eq.is_valid(OPERATORS2)))
