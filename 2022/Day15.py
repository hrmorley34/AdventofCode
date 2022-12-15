from __future__ import annotations
from puzzle_input import puzzle_input
from dataclasses import dataclass
import re

RE_SENSOR = re.compile(
    r"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"
)


@dataclass
class Sensor:
    pos: tuple[int, int]
    closest: tuple[int, int]

    @property
    def mh_distance(self) -> int:
        return abs(self.pos[0] - self.closest[0]) + abs(self.pos[1] - self.closest[1])

    @classmethod
    def from_string(cls, s: str):
        m = RE_SENSOR.match(s)
        assert m
        return cls(pos=(int(m[1]), int(m[2])), closest=(int(m[3]), int(m[4])))

    def get_invalidated_range_at_y(self, y: int) -> set[tuple[int, int]]:
        mh_distance = self.mh_distance
        s: set[tuple[int, int]] = set()
        dy = y - self.pos[1]
        xd = mh_distance - abs(dy)
        if xd >= 0:
            for dx in range(-xd, xd + 1):
                s.add((self.pos[0] + dx, y))
        return s

    # def get_invalidated_range(self) -> set[tuple[int, int]]:
    #     mh_distance = self.mh_distance
    #     s: set[tuple[int, int]] = set()
    #     for dy in range(-mh_distance, mh_distance + 1):
    #         xd = mh_distance - abs(dy)
    #         for dx in range(-xd, xd + 1):
    #             s.add((self.pos[0] + dx, self.pos[1] + dy))
    #     return s

    def is_position_invalid(self, pos: tuple[int, int]) -> bool:
        return abs(self.pos[0] - pos[0]) + abs(self.pos[1] - pos[1]) <= self.mh_distance

    def get_edge(self) -> set[tuple[int, int]]:
        """Returns the position just outside the invalid range"""
        mh_distance = self.mh_distance
        s: set[tuple[int, int]] = set()
        for dy in range(-mh_distance - 1, mh_distance + 2):
            xd = mh_distance - abs(dy) + 1
            for mul in (1, -1):
                s.add((self.pos[0] + xd * mul, self.pos[1] + dy))
        return s


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    SENSORS = [Sensor.from_string(s) for s in PUZZLE_INPUT]
    RANGE = (
        min(min(s.pos[0] for s in SENSORS), min(s.closest[0] for s in SENSORS)),
        max(max(s.pos[0] for s in SENSORS), max(s.closest[0] for s in SENSORS)),
        min(min(s.pos[1] for s in SENSORS), min(s.closest[1] for s in SENSORS)),
        max(max(s.pos[1] for s in SENSORS), max(s.closest[1] for s in SENSORS)),
    )

    Q_Y = 2_000_000
    # Q_Y = 10
    invalid_positions: set[tuple[int, int]] = set()
    excluded_positions: set[tuple[int, int]] = set()
    for s in SENSORS:
        invalid_positions |= s.get_invalidated_range_at_y(y=Q_Y)
        excluded_positions |= {s.closest, s.pos}
    invalid_positions -= excluded_positions
    print(f"Part 1: {len(invalid_positions)}")

    def position_is_invalid(p: tuple[int, int]):
        return any(s.is_position_invalid(p) for s in SENSORS)

    Q_R = (0, 4_000_000)
    # Q_R = (0, 20)
    check_positions: set[tuple[int, int]] = set()
    for s in SENSORS:
        check_positions |= {
            p
            for p in s.get_edge()
            if Q_R[0] <= p[0] <= Q_R[1]
            and Q_R[0] <= p[1] <= Q_R[1]
            and not position_is_invalid(p)
        }
        print(len(check_positions))
    # check_positions = {p for p in check_positions if not position_is_invalid(p)}
    assert len(check_positions) == 1
    p = next(iter(check_positions))
    print(f"Part 2: {p[0]*4_000_000+p[1]}")
