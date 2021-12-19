from __future__ import annotations

from collections import Counter, defaultdict
from dataclasses import dataclass
from enum import Flag
from puzzle_input import puzzle_input
from typing import Iterable, Literal, NewType


@dataclass(init=True, eq=True, frozen=True)
class UnknownPosition:
    x: int
    y: int
    z: int

    def map_to(self, p: UnknownPosition) -> set[PositionMapping]:
        return {PositionMapping.from_mapping(self, p, rot) for rot in ALL_ROTS}


KnownPosition = NewType("KnownPosition", UnknownPosition)


class RotationConst(Flag):
    plus = 0
    minus = 1

    plus_x = 2
    minus_x = 3
    plus_y = 4
    minus_y = 5
    plus_z = 8
    minus_z = 9

    all_evenpositive = plus_x | plus_y | plus_z


@dataclass(init=True, eq=True, frozen=True)
class Rotation:
    x: RotationConst
    y: RotationConst
    z: RotationConst

    # def __post_init__(self):
    #     assert (self.x ^ self.y ^ self.z) == RotationConst.all_evenpositive

    def apply(self, pos: UnknownPosition) -> UnknownPosition:
        d: dict[str, int] = dict()
        for value, rot in [(pos.x, self.x), (pos.y, self.y), (pos.z, self.z)]:
            if rot & RotationConst.minus:
                value = -value
            if rot & RotationConst.plus_x:
                d["x"] = value
            elif rot & RotationConst.plus_y:
                d["y"] = value
            elif rot & RotationConst.plus_z:
                d["z"] = value
            else:
                raise ValueError
        return UnknownPosition(**d)

    def get_xyz(self, key: Literal["x", "y", "z"] | RotationConst) -> RotationConst:
        is_rotconst = isinstance(key, RotationConst)
        if key == "x" or (is_rotconst and key & RotationConst.plus_x):
            return self.x
        elif key == "y" or (is_rotconst and key & RotationConst.plus_y):
            return self.y
        elif key == "z" or (is_rotconst and key & RotationConst.plus_z):
            return self.z
        else:
            raise ValueError

    def add(self, rot: Rotation) -> Rotation:
        d: dict[str, RotationConst] = dict()
        for key, thisv in [("x", self.x), ("y", self.y), ("z", self.z)]:
            rotv = rot.get_xyz(thisv)
            rotv ^= thisv & RotationConst.minus
            d[key] = rotv
        return Rotation(**d)

    def inverse(self) -> Rotation:
        d: dict[str, RotationConst] = dict()
        for key, thisv in [
            (RotationConst.plus_x, self.x),
            (RotationConst.plus_y, self.y),
            (RotationConst.plus_z, self.z),
        ]:
            neg = thisv & RotationConst.minus
            if thisv & RotationConst.plus_x:
                d["x"] = key | neg
            elif thisv & RotationConst.plus_y:
                d["y"] = key | neg
            elif thisv & RotationConst.plus_z:
                d["z"] = key | neg
            else:
                raise ValueError
        return Rotation(**d)

    @classmethod
    def default(cls) -> Rotation:
        return cls(RotationConst.plus_x, RotationConst.plus_y, RotationConst.plus_z)


ALL_ROTS: set[Rotation] = set()
for x in (RotationConst.plus_x, RotationConst.minus_x):
    for y in (RotationConst.plus_y, RotationConst.minus_y):
        for z in (RotationConst.plus_z, RotationConst.minus_z):
            try:
                Rotation(x, y, z)
            except Exception:
                continue
            else:
                for first in {x, y, z}:
                    for second in {x, y, z} - {first}:
                        for third in {x, y, z} - {first, second}:
                            ALL_ROTS.add(Rotation(first, second, third))


@dataclass(init=True, eq=True, frozen=True)
class PositionMapping:
    rot: Rotation
    dx: int
    dy: int
    dz: int

    @classmethod
    def from_mapping(
        cls, from_: UnknownPosition, to: UnknownPosition, rot: Rotation
    ) -> PositionMapping:
        newfrom = rot.apply(from_)
        dx = to.x - newfrom.x
        dy = to.y - newfrom.y
        dz = to.z - newfrom.z
        return cls(rot=rot, dx=dx, dy=dy, dz=dz)

    def apply(self, pos: UnknownPosition):
        pos = self.rot.apply(pos)
        return UnknownPosition(pos.x + self.dx, pos.y + self.dy, pos.z + self.dz)

    def add(self, m: PositionMapping) -> PositionMapping:
        newrot = self.rot.add(m.rot)
        dpos = m.rot.apply(UnknownPosition(self.dx, self.dy, self.dz))
        return PositionMapping(
            rot=newrot,
            dx=dpos.x + m.dx,
            dy=dpos.y + m.dy,
            dz=dpos.z + m.dz,
        )

    def add2(self, m: PositionMapping) -> PositionMapping:
        newrot = m.rot.add(self.rot)
        opos = UnknownPosition(m.dx, m.dy, m.dz)
        dpos = self.apply(m.apply(opos))
        mapping = {pm for pm in opos.map_to(dpos) if pm.rot == newrot}
        assert len(mapping) == 1
        return next(iter(mapping))

    def inverse(self) -> PositionMapping:
        return PositionMapping(
            rot=Rotation.default(), dx=-self.dx, dy=-self.dy, dz=-self.dz
        ).add(PositionMapping(rot=self.rot.inverse(), dx=0, dy=0, dz=0))


# class PositionMappingStack(PositionMapping):
#     stack: list[PositionMapping]

#     def __init__(self, stack: Iterable[PositionMapping]) -> None:
#         self.stack = list()
#         for item in stack:
#             if isinstance(item, PositionMappingStack):
#                 self.stack.extend(item.stack)
#             else:
#                 self.stack.append(item)

#     def __repr__(self) -> str:
#         return type(self).__name__ + "(" + ", ".join(map(repr, self.stack)) + ")"

#     def apply(self, pos: UnknownPosition) -> UnknownPosition:
#         for s in self.stack:
#             pos = s.apply(pos)
#         return pos

#     def inverse(self) -> PositionMappingStack:
#         return PositionMappingStack(m.inverse() for m in reversed(self.stack))


def find_sets(
    a: Iterable[UnknownPosition], b: Iterable[UnknownPosition], threshold: int = 12
) -> set[PositionMapping]:
    c: Counter[PositionMapping] = Counter()
    for posa in a:
        for posb in b:
            c.update(posa.map_to(posb))
    return {k for k in c if c[k] >= threshold}


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().split("\n\n")
    SCANNERS = {
        int(scanner.splitlines()[0][12:-4]): {
            UnknownPosition(*map(int, line.split(",")))
            for line in scanner.splitlines()[1:]
        }
        for scanner in PUZZLE_INPUT
    }

    from_: defaultdict[int, dict[int, PositionMapping]] = defaultdict(dict)
    for i1, scanner1 in SCANNERS.items():
        for i2, scanner2 in SCANNERS.items():
            if i1 == i2:
                continue

            s = find_sets(scanner2, scanner1)
            if len(s) == 1:
                from_[i1][i2] = next(iter(s))
            elif len(s) > 1:
                raise Exception
        print(f"Initial map: {i1}/{len(SCANNERS)}")
    print("Created initial map")

    from_zero: dict[int, PositionMapping] = {
        0: PositionMapping(
            Rotation.default(),
            0,
            0,
            0,
        )
    }
    while set(from_zero) < set(SCANNERS):
        for a, vs in from_.items():
            for b, v in vs.items():
                if b in from_zero:
                    continue
                if a == 0:
                    from_zero[b] = v
                elif a in from_zero:
                    # print("ADD")
                    # print(" ", a, from_zero[a])
                    # print(" ", b, v)
                    from_zero[b] = from_zero[a].add2(v)
                    # from_zero[b] = v.add(from_zero[a])
                    # print("  =>", from_zero[b])
        print(f"Map: {len(from_zero)}")
    print("Fully mapped")

    beacons: set[KnownPosition] = set()
    for key, mapping in from_zero.items():
        inv = mapping  # .inverse()
        for point in SCANNERS[key]:
            beacons.add(KnownPosition(inv.apply(point)))
    print("Part 1:", len(beacons))

    SCANNERS_LIST_IDS = list(SCANNERS)
    # DISTANCES: dict[frozenset[int], int] = {}
    max_distance: int = 0
    for i, scanner1 in enumerate(SCANNERS_LIST_IDS):
        pos1 = from_zero[scanner1]
        for scanner2 in SCANNERS_LIST_IDS[i + 1 :]:
            pos2 = from_zero[scanner2]
            distance = (
                abs(pos1.dx - pos2.dx) + abs(pos1.dy - pos2.dy) + abs(pos1.dz - pos2.dz)
            )
            # DISTANCES[frozenset((scanner1, scanner2))] = distance
            if distance > max_distance:
                max_distance = distance
    print("Part 2:", max_distance)
