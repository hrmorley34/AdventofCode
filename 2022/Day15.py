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
