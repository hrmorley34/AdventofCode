from dataclasses import dataclass
from math import ceil, floor, sqrt

from puzzle_input import puzzle_input


@dataclass
class Race:
    time: int
    distance: int

    def beat_record(self) -> int:
        # hold + travel = time
        # speed * travel > distance
        # speed = hold
        # => hold * (time - hold) > distance
        # => hold*time - hold**2 > distance
        # => hold**2 - hold*time < -distance
        # => (hold - 1/2 time)**2 - 1/4 time**2 < -distance
        # => (hold - 1/2 time)**2 < -distance + 1/4 time**2
        # => -sqrt(-distance + 1/4 time**2) < hold - 1/2 time
        #       < sqrt(-distance + 1/4 time**2)
        # => -sqrt(-distance + 1/4 time**2) + 1/2 time < hold
        #       < sqrt(-distance + 1/4 time**2) + 1/2 time
        root = sqrt(-self.distance + self.time**2 / 4)
        min_h = floor(1 / 2 * self.time - root) + 1
        assert min_h >= 0
        max_h = ceil(1 / 2 * self.time + root) - 1
        assert max_h <= self.time
        if max_h < min_h:
            return 0
        return max_h - min_h + 1


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    timestr, distancestr = PUZZLE_INPUT
    times = map(int, timestr.split()[1:])
    distances = map(int, distancestr.split()[1:])

    races = [Race(t, d) for t, d in zip(times, distances)]

    p = 1
    for r in races:
        print(r.beat_record())
        p *= r.beat_record()
    print("Part 1:", p)

    ktime = int("".join(timestr.split()[1:]))
    kdistance = int("".join(distancestr.split()[1:]))
    print("Part 2:", Race(ktime, kdistance).beat_record())
