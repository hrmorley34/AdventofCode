from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

from numpy import array, linalg
from puzzle_input import puzzle_input


@dataclass
class Vec3:
    x: int | float
    y: int | float
    z: int | float

    @classmethod
    def from_line(cls, line: str):
        x, y, z = map(int, line.split(", "))
        return cls(x, y, z)

    def __getitem__(self, i: int):
        return (self.x, self.y, self.z)[i]

    def __iter__(self):
        return iter((self.x, self.y, self.z))

    def __add__(self, o: Vec3) -> Vec3:
        return Vec3(self.x + o.x, self.y + o.y, self.z + o.z)

    def __neg__(self) -> Vec3:
        return Vec3(-self.x, -self.y, -self.z)

    def __sub__(self, o: Vec3) -> Vec3:
        return self + (-o)

    def __mul__(self, i: int | float | Vec3) -> Vec3:
        if isinstance(i, Vec3):
            return Vec3(self.x * i.x, self.y * i.y, self.z * i.z)
        return Vec3(self.x * i, self.y * i, self.z * i)

    def __rmul__(self, i: int | float | Vec3) -> Vec3:
        return self * i

    def __truediv__(self, i: int | float | Vec3) -> Vec3:
        if isinstance(i, Vec3):
            return Vec3(self.x / i.x, self.y / i.y, self.z / i.z)
        return Vec3(self.x / i, self.y / i, self.z / i)

    def __rtruediv__(self, i: int | float | Vec3) -> Vec3:
        if isinstance(i, Vec3):
            return Vec3(i.x / self.x, i.y / self.y, i.z / self.z)
        return Vec3(i / self.x, i / self.y, i / self.z)


@dataclass
class Hailstone:
    pos: Vec3
    vel: Vec3

    @classmethod
    def from_line(cls, line: str):
        pos, vel = map(Vec3.from_line, line.split(" @ "))
        return cls(pos, vel)

    def check_collision_xy(self, other: Hailstone, mins: int, maxs: int) -> bool:
        # pos1 + t1*vel1 = pos2 + t2*vel2
        # t1*vel1 - t2*vel2 = pos2 - pos1
        # [vel1 -vel2][t1] = pos2 - pos1
        #             [t2]
        dpos = other.pos - self.pos
        try:
            times = linalg.solve(
                array([[self.vel.x, -other.vel.x], [self.vel.y, -other.vel.y]]),
                array([dpos.x, dpos.y]),
            )
        except linalg.LinAlgError as err:
            if err.args == ("Singular matrix",):
                return False
            raise
        t1, t2 = times
        if t1 < 0 or t2 < 0:
            return False
        pos = self.pos + t1 * self.vel
        return (mins <= pos.x <= maxs) and (mins <= pos.y <= maxs)

    @classmethod
    def intercept_all(cls, stones: list[Hailstone]) -> Hailstone:
        if TYPE_CHECKING:
            Eq: Any = None
            solve: Any = None
            symbols: Any = None
        else:
            from sympy import Eq, solve, symbols

        p = Vec3(*symbols("px py pz"))
        v = Vec3(*symbols("vx vy vz"))
        ts = [(symbols(f"t{i}"), st.pos, st.vel) for i, st in enumerate(stones[:3], 1)]
        pairs = [(p + v * t, stp + stv * t) for t, stp, stv in ts]
        eqns = [Eq(lhs[i], rhs[i]) for lhs, rhs in pairs for i in range(3)]
        (soln,) = solve(eqns, (*p, *v, *(t[0] for t in ts)))
        return cls(Vec3(*map(int, soln[:3])), Vec3(*map(int, soln[3:6])))


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    stones = list(map(Hailstone.from_line, PUZZLE_INPUT))

    RANGE = (200000000000000, 400000000000000)
    print(
        "Part 1:",
        sum(
            stones[i].check_collision_xy(stones[j], RANGE[0], RANGE[1])
            for i in range(0, len(stones) - 1)
            for j in range(i + 1, len(stones))
        ),
    )

    print("Part 2:", sum(Hailstone.intercept_all(stones).pos))
