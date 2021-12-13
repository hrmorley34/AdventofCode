from __future__ import annotations

from dataclasses import dataclass
from puzzle_input import puzzle_input


@dataclass(init=True, eq=True, frozen=True)
class Point:
    x: int
    y: int

    def foldx(self, x: int) -> Point:
        if self.x > x:
            return Point(2 * x - self.x, self.y)
        return self

    def foldy(self, y: int) -> Point:
        if self.y > y:
            return Point(self.x, 2 * y - self.y)
        return self


@dataclass
class Map:
    points: set[Point]

    def foldx(self, x: int):
        self.points = {p.foldx(x) for p in self.points}

    def foldy(self, y: int):
        self.points = {p.foldy(y) for p in self.points}

    def fold(self, letter: str, value: int):
        if letter == "x":
            return self.foldx(value)
        elif letter == "y":
            return self.foldy(value)
        else:
            raise ValueError()

    def prettify(self) -> list[str]:
        minx = min(p.x for p in self.points)
        miny = min(p.y for p in self.points)
        maxx = max(p.x for p in self.points)
        maxy = max(p.y for p in self.points)

        text: list[str] = []
        for y in range(miny, maxy + 1):
            s = ""
            for x in range(minx, maxx + 1):
                s += "#" if Point(x, y) in self.points else "."
            text.append(s)
        return text


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().split("\n\n")
    POINTS = {
        Point(*map(int, pair.split(","))) for pair in PUZZLE_INPUT[0].splitlines()
    }
    FOLDS = [(s[11], int(s[13:])) for s in PUZZLE_INPUT[1].splitlines()]

    m = Map(POINTS)
    m.fold(*FOLDS[0])
    print("Part 1:", len(m.points))
    for fold in FOLDS[1:]:
        m.fold(*fold)
    print("Part 2:")
    print("\n".join(m.prettify()))
