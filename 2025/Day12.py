from dataclasses import dataclass
from functools import cache

from puzzle_input import puzzle_input
from tqdm import tqdm


@dataclass(frozen=True)
class Shape:
    shape: tuple[tuple[bool, ...], ...]

    @classmethod
    def from_lines(cls, lines: list[str]):
        return cls(tuple(tuple(c == "#" for c in line) for line in lines))

    def rotate(self) -> "Shape":
        # clockwise rotation 90deg
        return Shape(tuple(map(tuple, zip(*self.shape[::-1]))))

    def hflip(self) -> "Shape":
        return Shape(tuple(line[::-1] for line in self.shape))

    def vflip(self) -> "Shape":
        return Shape(self.shape[::-1])

    @cache
    def all_orientations(self) -> set["Shape"]:
        orien = {self}
        orien |= {x.rotate() for x in orien}
        orien |= {x.hflip() for x in orien}
        orien |= {x.vflip() for x in orien}
        return orien

    def area(self) -> int:
        return sum(map(sum, self.shape))


@cache
def try_pack(space: tuple[tuple[bool, ...], ...], shapes: tuple[Shape, ...]) -> bool:
    if not shapes:
        return True
    for y in range(len(space) - 2):
        for x in range(len(space[y]) - 2):
            for orien in shapes[0].all_orientations():
                mspace = list(map(list, space))
                valid = True
                for dy, line in enumerate(orien.shape):
                    for dx, cell in enumerate(line):
                        if mspace[y + dy][x + dx] and cell:
                            valid = False
                            break
                        mspace[y + dy][x + dx] |= cell
                    if not valid:
                        break
                if not valid:
                    continue
                if try_pack(tuple(map(tuple, mspace)), shapes[1:]):
                    return True
    return False


@dataclass
class Tree:
    size: tuple[int, int]
    presents: list[int]

    @classmethod
    def from_string(cls, line: str):
        size, pres = line.split(": ")
        w, h = size.split("x")
        return cls((int(w), int(h)), list(map(int, pres.split())))

    def area(self) -> int:
        return self.size[0] * self.size[1]

    def try_pack(self, presents: list[Shape]) -> bool:
        if (
            sum(
                count * present.area()
                for count, present in zip(self.presents, presents)
            )
            > self.area()
        ):
            return False
        return try_pack(
            ((False,) * self.size[0],) * self.size[1],
            sum(
                (count * (present,) for count, present in zip(self.presents, presents)),
                (),
            ),
        )


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    *shapes_s, trees_s = PUZZLE_INPUT.split("\n\n")
    shapes = [Shape.from_lines(shape.splitlines()[1:]) for shape in shapes_s]
    trees = list(map(Tree.from_string, trees_s.splitlines()))

    count = 0
    for tree in tqdm(trees):
        count += tree.try_pack(shapes)
    print("Part 1:", count)
