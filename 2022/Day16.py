import heapq
import re
from dataclasses import dataclass, field
from itertools import combinations
from typing import Any

from puzzle_input import puzzle_input

RE_VALVE = re.compile(
    r"^Valve (\w{2}) has flow rate=(\d+); "
    r"tunnels? leads? to valves? (\w{2}(?:, \w{2})*)$"
)


@dataclass
class Valve:
    name: str
    rate: int
    connections: set[str]

    @classmethod
    def from_str(cls, s: str):
        m = RE_VALVE.match(s)
        assert m, s
        return cls(
            name=m[1],
            rate=int(m[2]),
            connections=set(m[3].split(", ")),
        )


@dataclass()
class ValveDijkstraToken:
    position: str
    open_valves: frozenset[str]
    released_pressure: int
    time: int

    via: "ValveDijkstraToken | None" = field(repr=False)
    valves: dict[str, Valve] = field(repr=False)

    flow_rate: int = field(init=False, repr=True)

    def __post_init__(self) -> None:
        self.flow_rate = sum(self.valves[v].rate for v in self.open_valves)

    @property
    def sort_key(self):
        return self.time, -self.released_pressure, -self.flow_rate

    def __lt__(self, o: Any) -> bool:
        if isinstance(o, ValveDijkstraToken):
            return self.sort_key < o.sort_key
        return NotImplemented


def valve_dijkstra(valves: dict[str, Valve]) -> int:
    queue: list[ValveDijkstraToken] = []
    t = ValveDijkstraToken("AA", frozenset(), 0, 0, None, valves)
    seen: set[tuple[str, int, frozenset[str]]] = set()
    seen_times: set[int] = set()
    while t.time < 30:
        new_pressure = t.released_pressure + t.flow_rate
        new_time = t.time + 1
        for neighbour in valves[t.position].connections:
            if (neighbour, new_time, t.open_valves) in seen:
                continue
            heapq.heappush(
                queue,
                ValveDijkstraToken(
                    neighbour, t.open_valves, new_pressure, new_time, t, valves
                ),
            )
        if (t.position, new_time, t.open_valves | {t.position}) not in seen:
            # Either open this valve, or do nothing here
            heapq.heappush(
                queue,
                ValveDijkstraToken(
                    t.position,
                    t.open_valves | {t.position},
                    new_pressure,
                    new_time,
                    t,
                    valves,
                ),
            )

        seen.add((t.position, t.time, t.open_valves))
        for i in range(0, len(t.open_valves)):
            for combo in combinations(t.open_valves, i):
                seen.add((t.position, t.time, frozenset(combo)))

        if len(seen) % 10_000 == 0:
            print(f"{len(seen)}; t={t.time}")

        while (t.position, t.time, t.open_valves) in seen:
            t = heapq.heappop(queue)

        if t.time not in seen_times:
            seen.clear()
            queue = queue[: int(1.7 ** (40 - t.time))]
        seen_times.add(t.time)

    # pt = t
    # while pt:
    #     print(pt)
    #     pt = pt.via
    return t.released_pressure


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    VALVES = [Valve.from_str(s) for s in PUZZLE_INPUT]
    VALVED = {v.name: v for v in VALVES}

    pr = valve_dijkstra(VALVED)
    print(f"Part 1: {pr}")
