from __future__ import annotations

from dataclasses import dataclass
import re
from typing import Any


TARGET_RE = re.compile(
    r"target area: x=(?P<x0>-?\d+)\.\.(?P<x1>-?\d+), y=(?P<y0>-?\d+)\.\.(?P<y1>-?\d+)"
)


@dataclass(init=True, eq=True, frozen=True)
class Vector:
    x: int
    y: int

    def __add__(self, right: Any) -> Vector:
        if isinstance(right, Vector):
            return Vector(self.x + right.x, self.y + right.y)
        return NotImplemented

    def __neg__(self) -> Vector:
        return Vector(-self.x, -self.y)

    def __sub__(self, right: Any) -> Vector:
        if isinstance(right, Vector):
            return self + (-right)
        return NotImplemented


class Probe:
    position: Vector
    velocity: Vector

    def __init__(self, velocity: Vector) -> None:
        self.position = Vector(0, 0)
        self.velocity = velocity

    def step(self):
        self.position += self.velocity

        # Drag
        if self.velocity.x > 0:
            self.velocity += Vector(-1, 0)
        elif self.velocity.x < 0:
            self.velocity += Vector(1, 0)

        # Gravity
        self.velocity += Vector(0, -1)

    def hits_range(self, r: XYRange) -> bool:
        while (self.position.x <= r.maxx or self.velocity.x < 0) and (
            self.position.y >= r.miny or self.velocity.y > 0
        ):
            if self.position in r:
                return True
            self.step()
        return False


def max_height(initial_velocity: Vector) -> int:
    if initial_velocity.y <= 0:
        return initial_velocity.y
    return initial_velocity.y * (initial_velocity.y + 1) // 2


@dataclass(init=True)
class XYRange:
    minx: int
    maxx: int
    miny: int
    maxy: int

    def __contains__(self, v: Vector | Any) -> bool:
        if isinstance(v, Vector):
            return self.minx <= v.x <= self.maxx and self.miny <= v.y <= self.maxy
        return False

    @classmethod
    def from_text(cls, text: str) -> XYRange:
        m = TARGET_RE.match(text)
        assert m is not None
        return cls(
            minx=int(m["x0"]), maxx=int(m["x1"]), miny=int(m["y0"]), maxy=int(m["y1"])
        )

    def hard_lower_bound_y(self) -> int:
        "(inclusive)"
        # for negative y
        # 0 + y >= miny
        return self.miny

    def hard_upper_bound_y(self) -> int:
        "(inclusive)"
        # for positive y
        # 0 - (y+1) >= miny
        # -y -1 >= miny
        # -miny >= y +1
        # -miny -1 >= y
        return -self.miny - 1

    def hard_lower_bound_x(self) -> int:
        "(inclusive)"
        # x + (x-1) + (x-2) + ... + 0 < minx
        # x * (x+1) // 2 < minx
        # x * (x+1) < 2 * minx
        # x**2 + x - 2*minx < 0
        # two solutions, one is almost always <0
        # so x = sqrt(8*minx +1)/2 -1/2
        # floored is lower limit
        return int(((8 * self.minx + 1) ** (1 / 2)) / 2 - 1 / 2)

    def hard_upper_bound_x(self) -> int:
        "(exclusive)"
        return self.maxx + 1


if __name__ == "__main__":
    PUZZLE_INPUT = input()
    r = XYRange.from_text(PUZZLE_INPUT)

    count: int = 0

    current_max: int = r.miny - 1
    current_vel: Vector | None = None
    for x in range(r.hard_lower_bound_x(), r.hard_upper_bound_x()):
        for y in range(r.hard_lower_bound_y(), r.hard_upper_bound_y() + 1):
            vel = Vector(x, y)

            # More efficient for part 1
            # m = max_height(vel)
            # if m > current_max:
            #     if Probe(vel).hits_range(r):
            #     if m > current_max:
            #         current_max = m
            #         current_vel = vel

            # Covers all for part 2
            if Probe(vel).hits_range(r):
                count += 1
                m = max_height(vel)
                if m > current_max:
                    current_max = m
                    current_vel = vel
    print("Part 1:", current_max)
    print("Part 2:", count)
