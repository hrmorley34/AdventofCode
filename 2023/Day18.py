import re
from typing import NamedTuple

# from Day10 import fill_sides
from puzzle_input import puzzle_input

DIR_TUPLES = {"U": (0, -1), "D": (0, 1), "L": (-1, 0), "R": (1, 0)}
DIR_HEXDIGITS = {"0": "R", "1": "D", "2": "L", "3": "U"}


class DigPlanLine(NamedTuple):
    dir: str
    length: int
    colour: str = ""

    @classmethod
    def from_line(cls, line: str):
        r = re.match(r"([UDLR]) (\d+) \(#(\w{6})\)", line)
        assert r is not None, line
        return cls(r[1], int(r[2]), r[3])

    @property
    def dir_tuple(self) -> tuple[int, int]:
        return DIR_TUPLES[self.dir]

    def swap(self):
        return DigPlanLine(
            DIR_HEXDIGITS[self.colour[-1]], int(self.colour[:-1], 16), ""
        )


def create_zero_aligned_edge(
    lines: list[DigPlanLine]
) -> tuple[
    tuple[int, int],
    dict[tuple[int, int], tuple[int, int]],
    tuple[int, int],
]:
    cx, cy = (0, 0)
    maps: dict[tuple[int, int], tuple[int, int]] = {}
    minx, miny, maxx, maxy = 0, 0, 0, 0

    for line in lines:
        dx, dy = line.dir_tuple
        for _ in range(line.length):
            newx, newy = cx + dx, cy + dy
            maps[cx, cy] = (newx, newy)
            (cx, cy) = (newx, newy)
        minx, miny = min(minx, cx), min(miny, cy)
        maxx, maxy = max(maxx, cx), max(maxy, cy)
    assert (cx, cy) == (0, 0)

    return (
        (-minx, -miny),
        {
            (f[0] - minx, f[1] - miny): (t[0] - minx, t[1] - miny)
            for f, t in maps.items()
        },
        (maxx - minx + 1, maxy - miny + 1),
    )


def pprint_lines(lines: list[DigPlanLine]) -> None:
    _, maps, dim = create_zero_aligned_edge(lines)
    for y in range(0, dim[1]):
        s = ""
        for x in range(0, dim[0]):
            s += "#" if (x, y) in maps else "."
        print(s)
    print()


def reduce_edges_u(
    e1: DigPlanLine, e2: DigPlanLine, e3: DigPlanLine
) -> tuple[list[DigPlanLine], int] | None:
    j = "".join((e1.dir, e2.dir, e3.dir))
    if j in (
        "DLU",
        "RDL",
        "LUR",
        "URD",
    ):
        darea = +1
    elif j in (
        "DRU",
        "RUL",
        "LDR",
        "ULD",
    ):
        darea = -1
    else:
        # Find U-shapes
        return None

    new_edges = [e2]
    if e1.length > e3.length:
        new_edges.insert(0, DigPlanLine(e1.dir, e1.length - e3.length))
    elif e1.length < e3.length:
        new_edges.append(DigPlanLine(e3.dir, e3.length - e1.length))
    # else: Equal. No line segment needed

    # 1|
    # 2|      +(------)
    # 3|      |1
    # 4+------+2
    #  01234567
    # leaves out a 2*8 area to become
    # 1|
    # 2+------+------
    #  ########
    #  ########
    removed_area = min(e1.length, e3.length) * (e2.length + 1 * darea)

    return new_edges, removed_area * darea


def reduce_edges_line(
    e1: DigPlanLine, e2: DigPlanLine
) -> tuple[DigPlanLine, int] | None:
    if e1.dir == e2.dir:
        neg = False
    elif "".join((e1.dir, e2.dir)) in ("DU", "UD", "LR", "RL"):
        neg = True
    else:
        return None
    if neg:
        new_length = e1.length - e2.length
        return (
            DigPlanLine(e2.dir if new_length < 0 else e1.dir, abs(new_length)),
            max(e1.length, e2.length) - abs(new_length),
        )
    else:
        return DigPlanLine(e1.dir, e1.length + e2.length), 0


def reduce_final(lines: list[DigPlanLine]) -> int | None:
    if len(lines) not in (1, 2):
        return None
    elif len(lines) == 1:
        assert lines[0].length == 0
        return 1
    l1, l2 = lines
    assert {l1, l2} in (set("UD"), set("LR")), lines
    return l1.length + 1  # just the length of the line itself, including the end points


def reduce_edges(lines: list[DigPlanLine]) -> int:
    area = 0
    lines = lines.copy()
    new_lines: list[DigPlanLine] = []
    # print(area)
    # pprint_lines(lines)
    while len(lines) > 2:
        i = 0
        while i < len(lines) - 2:
            u_result = reduce_edges_u(*lines[i : i + 3])
            if u_result is not None:
                segments, darea = u_result
                new_lines.extend(segments)
                area += darea
                i += 3
                continue

            new_lines.append(lines[i])
            i += 1
        while i < len(lines):
            new_lines.append(lines[i])
            i += 1

        lines, new_lines = new_lines, []
        # print(area)
        # pprint_lines(lines)

        prev = lines[0]
        i = 1
        while i < len(lines):
            l_result = reduce_edges_line(prev, lines[i])
            if l_result is not None:
                prev = l_result[0]
                area += l_result[1]
            else:
                new_lines.append(prev)
                prev = lines[i]
            i += 1
        new_lines.append(prev)

        lines, new_lines = new_lines, []
        # print(area)
        # pprint_lines(lines)

    darea = reduce_final(lines)
    assert darea is not None, lines
    return area + darea


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    lines = list(map(DigPlanLine.from_line, PUZZLE_INPUT))
    # start, conn, dim = create_zero_aligned_edge(lines)
    # area = fill_sides(start, conn, dim) + len(conn)
    area = reduce_edges(lines)
    print("Part 1:", area)

    lines2 = list(map(DigPlanLine.swap, lines))
    # start2, conn2, dim2 = create_zero_aligned_edge(lines2)
    # area2 = fill_sides(start2, conn2, dim2) + len(conn2)
    area2 = reduce_edges(lines2)
    print("Part 2:", area2)
