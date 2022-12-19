import re
from typing import Any, NamedTuple

from puzzle_input import puzzle_input

BLUEPRINT_RE = re.compile(
    r"^Blueprint (\d+):\s+"
    r"Each ore robot costs (\d+) ore.\s+"
    r"Each clay robot costs (\d+) ore.\s+"
    r"Each obsidian robot costs (\d+) ore and (\d+) clay.\s+"
    r"Each geode robot costs (\d+) ore and (\d+) obsidian.$"
)


class Materials(NamedTuple):
    ore: int
    clay: int = 0
    obsidian: int = 0
    geode: int = 0

    def __add__(self, o: "Materials | Any") -> "Materials":
        if isinstance(o, Materials):
            return Materials(*(a + b for a, b in zip(self, o)))
        return NotImplemented

    def __neg__(self) -> "Materials":
        return Materials(*(-x for x in self))

    def __sub__(self, o: "Materials | Any") -> "Materials":
        if isinstance(o, Materials):
            return self + (-o)
        return NotImplemented

    def __mul__(self, o: int | Any) -> "Materials":
        if isinstance(o, int):
            return Materials(*(x * o for x in self))
        return NotImplemented

    def __le__(self, o: "Materials | Any") -> bool:
        if isinstance(o, Materials):
            return all(a <= b for a, b in zip(self, o))
        return NotImplemented

    def __ge__(self, o: "Materials | Any") -> bool:
        if isinstance(o, Materials):
            return all(a >= b for a, b in zip(self, o))
        return NotImplemented


class Robot(NamedTuple):
    cost: Materials
    generates: Materials


class Blueprint(NamedTuple):
    id: int
    robots: tuple[Robot, ...]

    @classmethod
    def from_str(cls, s: str):
        m = BLUEPRINT_RE.match(s)
        assert m
        return cls(
            int(m[1]),
            (
                Robot(Materials(int(m[2])), Materials(1)),
                Robot(Materials(int(m[3])), Materials(0, 1)),
                Robot(Materials(int(m[4]), int(m[5])), Materials(0, 0, 1)),
                Robot(Materials(int(m[6]), 0, int(m[7])), Materials(0, 0, 0, 1)),
            ),
        )

    def get_quality(self, time: int = 24) -> int:
        return self.id * self.open_max(
            time=time, money=Materials(0), gen=self.robots[0].generates, depth=0
        )

    def open_max(self, time: int, money: Materials, gen: Materials, depth: int) -> int:
        submax: int = 0
        available_robots = set(self.robots)
        while time > 1:
            for r in available_robots.copy():
                if r.cost <= money:
                    submax = max(
                        submax,
                        self.open_max(
                            time - 1, money - r.cost + gen, gen + r.generates, depth + 1
                        ),
                    )
                    available_robots.discard(r)
            time -= 1
            money += gen
        time -= 1
        money += gen
        assert time == 0
        return max(submax, money.geode)


if __name__ == "__main__":
    # PUZZLE_INPUT = puzzle_input().split("\n\n")
    PUZZLE_INPUT = puzzle_input().splitlines()
    BLUEPRINTS = [Blueprint.from_str(bp) for bp in PUZZLE_INPUT]

    TIME = 24
    qsum = 0
    for i, bp in enumerate(BLUEPRINTS):
        print(f"{i:2}/{len(BLUEPRINTS):2}")
        qsum += bp.get_quality(TIME)
    print(f"{len(BLUEPRINTS):2}/{len(BLUEPRINTS):2}")
    print(f"Part 1: {qsum}")
