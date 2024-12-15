from dataclasses import dataclass
from typing import Self

from puzzle_input import puzzle_input

DIRECTIONS = {"<": (-1, 0), "^": (0, -1), ">": (1, 0), "v": (0, 1)}


@dataclass
class Map:
    robot: tuple[int, int]
    size: tuple[int, int]
    boxes: set[tuple[int, int]]
    walls: set[tuple[int, int]]

    @classmethod
    def from_string(cls, lines: list[str]) -> Self:
        assert len(set(map(len, lines))) == 1
        assert all(all(c == "#" for c in line) for line in (lines[0], lines[-1]))
        assert all(all(c == "#" for c in (line[0], line[-1])) for line in (lines[1:-1]))
        robot = (-1, -1)
        size = (len(lines[0]), len(lines))
        boxes: set[tuple[int, int]] = set()
        walls: set[tuple[int, int]] = set()
        for y, line in enumerate(lines[1:-1], 1):
            for x, c in enumerate(line[1:-1], 1):
                if c == "@":
                    assert robot == (-1, -1)
                    robot = (x, y)
                elif c == "O":
                    boxes.add((x, y))
                elif c == "#":
                    walls.add((x, y))
                else:
                    assert c == ".", c
        assert robot != (-1, -1)
        return cls(robot=robot, size=size, boxes=boxes, walls=walls)

    def in_range(self, pos: tuple[int, int]) -> bool:
        return 1 <= pos[0] < self.size[0] - 1 and 1 <= pos[1] < self.size[1] - 1

    def move(self, d: tuple[int, int]):
        pos = self.robot
        boxes = 0
        can_push = None
        while can_push is None:
            pos = pos[0] + d[0], pos[1] + d[1]
            if not self.in_range(pos) or pos in self.walls:
                can_push = False
            elif pos in self.boxes:
                boxes += 1
            else:  # pos is empty
                can_push = True
        if can_push:
            if boxes:
                self.boxes.remove((self.robot[0] + d[0], self.robot[1] + d[1]))
                self.boxes.add(pos)
            self.robot = self.robot[0] + d[0], self.robot[1] + d[1]

    def print(self) -> None:
        print("#" * self.size[0])
        for y in range(1, self.size[1] - 1):
            print(
                "#"
                + "".join(
                    "@"
                    if (x, y) == self.robot
                    else "#"
                    if (x, y) in self.walls
                    else "O"
                    if (x, y) in self.boxes
                    else "."
                    for x in range(1, self.size[0] - 1)
                )
                + "#"
            )
        print("#" * self.size[0])

    def sum_gps(self) -> int:
        return sum(y * 100 + x for x, y in self.boxes)


class DoubleMap(Map):
    @classmethod
    def from_string(cls, lines: list[str]) -> Self:
        m = super().from_string(lines)
        return cls(
            robot=(m.robot[0] * 2, m.robot[1]),
            size=(m.size[0] * 2, m.size[1]),
            boxes={(x * 2, y) for x, y in m.boxes},
            walls={(x * 2, y) for x, y in m.walls}
            | {(x * 2 + 1, y) for x, y in m.walls},
        )

    def in_range(self, pos: tuple[int, int]) -> bool:
        return 2 <= pos[0] < self.size[0] - 2 and 1 <= pos[1] < self.size[1] - 1

    def move(self, d: tuple[int, int]):
        positions = {(self.robot[0] + d[0], self.robot[1] + d[1])}
        boxes: set[tuple[int, int]] = set()
        can_push = None
        while can_push is None:
            if any(not self.in_range(pos) or pos in self.walls for pos in positions):
                can_push = False
            else:
                new_boxes = {
                    (x, y)
                    for x, y in self.boxes
                    if any((x + i, y) in positions for i in range(2))
                } - boxes
                if new_boxes:
                    boxes.update(new_boxes)
                    positions = {(x + d[0], y + d[1]) for x, y in new_boxes} | {
                        (x + d[0] + 1, y + d[1]) for x, y in new_boxes
                    }
                else:
                    can_push = True
        if can_push:
            if boxes:
                self.boxes.difference_update(boxes)
                self.boxes.update((x + d[0], y + d[1]) for x, y in boxes)
            self.robot = self.robot[0] + d[0], self.robot[1] + d[1]

    def print(self) -> None:
        print("#" * self.size[0])
        for y in range(1, self.size[1] - 1):
            print(
                "##"
                + "".join(
                    "@"
                    if (x, y) == self.robot
                    else "#"
                    if (x, y) in self.walls
                    else "["
                    if (x, y) in self.boxes
                    else "]"
                    if (x - 1, y) in self.boxes
                    else "."
                    for x in range(2, self.size[0] - 2)
                )
                + "##"
            )
        print("#" * self.size[0])


if __name__ == "__main__":
    m, i = puzzle_input().split("\n\n")
    MAP = m.splitlines()
    INSTRUCTIONS = i.replace("\n", "")

    map1 = Map.from_string(MAP)
    for inst in INSTRUCTIONS:
        # map1.print()
        # print()
        # print(inst)
        map1.move(DIRECTIONS[inst])
    # map1.print()
    print("Part 1:", map1.sum_gps())

    map2 = DoubleMap.from_string(MAP)
    for inst in INSTRUCTIONS:
        # map2.print()
        # print()
        # print(inst)
        map2.move(DIRECTIONS[inst])
    # map2.print()
    print("Part 2:", map2.sum_gps())
