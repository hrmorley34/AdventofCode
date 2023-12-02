from collections import Counter
from enum import Enum
from typing import NamedTuple

from puzzle_input import puzzle_input


class Colour(Enum):
    red = "red"
    green = "green"
    blue = "blue"


class Line(NamedTuple):
    number: int
    cubes: list[list[tuple[Colour, int]]]

    @classmethod
    def from_line(cls, s: str):
        game, cubes = s.split(": ")
        number = int(game[len("Game ") :])
        cubelist = [
            [
                (Colour(colour), int(count))
                for count, colour in map(lambda s: s.split(" "), part.split(", "))
            ]
            for part in cubes.split("; ")
        ]
        return cls(number=number, cubes=cubelist)

    def maximums(self) -> Counter[Colour]:
        c: Counter[Colour] = Counter()
        for part in self.cubes:
            # subc: Counter[Colour] = Counter()
            for col, i in part:
                #     subc[col] += i
                # for col, i in subc.items():
                c[col] = max(c[col], i)
        return c

    def power(self) -> int:
        m = self.maximums()
        return m[Colour.red] * m[Colour.green] * m[Colour.blue]


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    lines = list(map(Line.from_line, PUZZLE_INPUT))
    TARGET_1 = Counter({Colour.red: 12, Colour.green: 13, Colour.blue: 14})

    print("Part 1:", sum(line.number for line in lines if line.maximums() < TARGET_1))
    print("Part 2:", sum(line.power() for line in lines))
