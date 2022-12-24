import re
import heapq
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
    ore: int = 0
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

    def __mul__(self, o: "int | Any") -> "Materials":
        if isinstance(o, int):
            return Materials(*(x * o for x in self))
        return NotImplemented

    def can_buy(self, cost: "Materials") -> bool:
        return all(a >= b for a, b in zip(self, cost))

    @property
    def sort_key(self):
        return tuple(reversed(-self))


class Robot(NamedTuple):
    cost: Materials
    generates: Materials


class Token(NamedTuple):
    time: int
    money: Materials
    gen: Materials

    @property
    def sort_key(self):
        return self.gen.sort_key, self.money.sort_key, self.time

    def __lt__(self, o: "Token | Any") -> bool:
        if isinstance(o, Token):
            return self.sort_key < o.sort_key
        return NotImplemented


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
                Robot(Materials(ore=int(m[2])), Materials(ore=1)),
                Robot(Materials(ore=int(m[3])), Materials(clay=1)),
                Robot(Materials(ore=int(m[4]), clay=int(m[5])), Materials(obsidian=1)),
                Robot(Materials(ore=int(m[6]), obsidian=int(m[7])), Materials(geode=1)),
            ),
        )

    def get_quality(self, time: int = 24) -> int:
        return self.id * self.open_max(time)

    def estimate_geode_limit(self, gen: Materials, money: Materials, time: int) -> int:
        "Produces an upper limit for geode production"
        est_gen = gen
        est_money = money
        for _ in range(time):
            for robot in self.robots:
                if est_money.can_buy(robot.cost):
                    est_gen += robot.generates
            est_money += est_gen
        return est_money.geode

    def open_max(self, itime: int) -> int:
        top = 0
        queue = [Token(time=itime, money=Materials(), gen=self.robots[0].generates)]
        counter = 0
        max_costs = Materials(*map(max, zip(*(r.cost for r in self.robots))))
        max_costs = Materials(
            ore=max_costs.ore,
            clay=max_costs.clay,
            obsidian=max_costs.obsidian,
            geode=1_000_000_000,  # big number
        )
        while queue:
            time, money, gen = heapq.heappop(queue)
            if self.estimate_geode_limit(gen, money, time) <= top:
                continue

            available_robots = {
                # don't buy robots which generate faster than we can spend
                r
                for r in self.robots
                if all(
                    map(lambda ab: ab[0] <= ab[1], zip(gen + r.generates, max_costs))
                )
            }
            while time > 1:
                for r in available_robots.copy():
                    if money.can_buy(r.cost):
                        heapq.heappush(
                            queue,
                            Token(
                                time=time - 1,
                                money=money - r.cost + gen,
                                gen=gen + r.generates,
                            ),
                        )
                        available_robots.discard(r)
                time -= 1
                money += gen

            if time == 1:
                time -= 1
                money += gen
                top = max(top, money.geode)
            counter += 1
            if counter % 500_000 == 0:
                print(counter, len(queue), top)
        return top


if __name__ == "__main__":
    # PUZZLE_INPUT = puzzle_input().split("\n\n")
    PUZZLE_INPUT = puzzle_input().splitlines()
    BLUEPRINTS = [Blueprint.from_str(bp) for bp in PUZZLE_INPUT]

    TIME1 = 24
    qsum = 0
    for i, bp in enumerate(BLUEPRINTS):
        print(f"{i:2}/{len(BLUEPRINTS):2}")
        q = bp.get_quality(TIME1)
        qsum += q
        print(q)
    print(f"{len(BLUEPRINTS):2}/{len(BLUEPRINTS):2}")
    print(f"Part 1: {qsum}")

    TIME2 = 32
    LIM2 = 3
    qprod = 1
    for i, bp in enumerate(BLUEPRINTS[:LIM2]):
        print(f"{i}/{LIM2}")
        q = bp.open_max(TIME2)
        qprod *= q
        print(q)
    print(f"{LIM2}/{LIM2}")
    print(f"Part 2: {qprod}")
