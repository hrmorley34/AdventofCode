from typing import NamedTuple

from puzzle_input import puzzle_input


class Position(NamedTuple):
    x: int
    y: int

    @classmethod
    def from_string(cls, s: str):
        return cls(*map(int, s.split(",")))


def area(a: Position, b: Position) -> int:
    return (abs(a.x - b.x) + 1) * (abs(a.y - b.y) + 1)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    positions = list(map(Position.from_string, PUZZLE_INPUT.splitlines()))
    pairs = [(a, b) for i, a in enumerate(positions) for b in positions[i + 1 :]]

    print("Part 1:", max(area(a, b) for a, b in pairs))
