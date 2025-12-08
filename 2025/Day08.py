from math import prod
from typing import NamedTuple

from puzzle_input import puzzle_input


class Position(NamedTuple):
    x: int
    y: int
    z: int

    @classmethod
    def from_string(cls, s: str):
        return cls(*map(int, s.split(",")))

    def dist(self, other: "Position") -> float:
        return (
            (self.x - other.x) ** 2 + (self.y - other.y) ** 2 + (self.z - other.z) ** 2
        ) ** 0.5


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    P1_COUNT = 1000
    positions = list(map(Position.from_string, PUZZLE_INPUT.splitlines()))
    circuits = {p: {p} for p in positions}

    pairs = sorted(
        ((a, b) for i, a in enumerate(positions) for b in positions[i + 1 :]),
        key=lambda ab: ab[0].dist(ab[1]),
    )

    for a, b in pairs[:P1_COUNT]:
        if circuits[a] != circuits[b]:
            merged = circuits[a] | circuits[b]
            for p in merged:
                circuits[p] = merged
    fcircuits = {frozenset(c) for c in circuits.values()}
    print("Part 1:", prod(sorted(map(len, fcircuits), reverse=True)[:3]))

    for a, b in pairs[P1_COUNT:]:
        if circuits[a] != circuits[b]:
            merged = circuits[a] | circuits[b]
            for p in merged:
                circuits[p] = merged

            if len(merged) == len(positions):
                print("Part 2:", a.x * b.x)
                break
    else:
        raise Exception
