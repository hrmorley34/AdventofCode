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


def is_green(a: Position, b: Position, positions: list[Position]) -> bool:
    xmin, xmax = min(a.x, b.x), max(a.x, b.x)
    ymin, ymax = min(a.y, b.y), max(a.y, b.y)
    if any(
        xmin + 1 <= p.x <= xmax - 1 and ymin + 1 <= p.y <= ymax - 1 for p in positions
    ):
        return False
    if any(
        (
            xmin + 1 <= p.x == q.x <= xmax - 1
            and min(p.y, q.y) <= ymin
            and max(p.y, q.y) >= ymax
        )
        or (
            ymin + 1 <= p.y == q.y <= ymax - 1
            and min(p.x, q.x) <= xmin
            and max(p.x, q.x) >= xmax
        )
        for i, p in enumerate(positions)
        for q in (positions[(i + 1) % len(positions)],)
    ):
        return False
    return True


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    positions = list(map(Position.from_string, PUZZLE_INPUT.splitlines()))
    pairs = [(a, b) for i, a in enumerate(positions) for b in positions[i + 1 :]]

    positions_by_area = sorted(((area(a, b), a, b) for a, b in pairs), reverse=True)

    print("Part 1:", positions_by_area[0][0])

    rectpositions = positions.copy()
    for i in range(len(rectpositions) - 2, 0, -1):
        b, p, a = rectpositions[i - 1 : i + 2]
        if (b.x == p.x == a.x and (p.y - b.y) * (p.y - a.y) >= 0) or (
            b.y == p.y == a.y and (p.x - b.x) * (p.x - a.x) >= 0
        ):
            rectpositions.pop(i)

    print(
        "Part 2:",
        next(ar for ar, a, b in positions_by_area if is_green(a, b, rectpositions)),
    )
