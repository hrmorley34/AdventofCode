import re
from dataclasses import dataclass
from typing import NamedTuple, Iterable
from functools import reduce
from operator import or_
from puzzle_input import puzzle_input

RE_VALVE = re.compile(
    r"^Valve (\w{2}) has flow rate=(\d+); "
    r"tunnels? leads? to valves? (\w{2}(?:, \w{2})*)$"
)


def bit_iter(i: int):
    s = 0
    while i:
        if i & 1:
            yield 1 << s
        i >>= 1
        s += 1


def or_all(i: Iterable[int]) -> int:
    return reduce(or_, i, 0)


class Valve(NamedTuple):
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

    def to_connections(self, mapping: dict[str, int]) -> tuple[int, int]:
        return mapping[self.name], or_all(mapping[n] for n in self.connections)

    def to_rate(self, mapping: dict[str, int]) -> tuple[int, int]:
        return mapping[self.name], self.rate


@dataclass()
class Token:
    position: int
    open_valves: int
    released_pressure: int
    rem_time: int


def get_shortest_paths(valves: dict[int, int]) -> dict[int, dict[int, int]]:
    # Floyd-Warshall
    dist = {k1: {k2: float("inf") for k2 in valves} for k1 in valves}
    for u, conn in valves.items():
        for v in bit_iter(conn):
            dist[u][v] = 1  # The weight of the edge (u, v)
    for v in valves:
        dist[v][v] = 0
    for k in valves:
        for i in valves:
            for j in valves:
                if dist[i][j] > dist[i][k] + dist[k][j]:
                    dist[i][j] = dist[i][k] + dist[k][j]
    assert all(all(isinstance(i, int) for i in d.values()) for d in dist.values())
    return dist  # type: ignore


def recursive_depthfirst(
    rates: dict[int, int],
    distances: dict[int, dict[int, int]],
    initial: Token,
    depth: int = 0,
) -> Token:
    local_dist = distances[initial.position]
    valvenames = or_all(rates)
    best_solution = initial
    slncount = 0
    for v in bit_iter(valvenames ^ initial.open_valves):
        td = local_dist[v] + 1  # +1 for switching valve
        new_time = initial.rem_time - td
        new_pressure = initial.released_pressure + rates[v] * new_time
        if new_time < 0:
            continue
        nt = Token(v, initial.open_valves | v, new_pressure, new_time)
        if new_time == 0:
            best_solution = max(best_solution, nt, key=lambda t: t.released_pressure)
        else:
            best_solution = max(
                best_solution,
                recursive_depthfirst(rates, distances, nt, depth=depth + 1),
                key=lambda t: t.released_pressure,
            )
        slncount += 1
    if slncount == 0:  # initial.rem_time > 0:
        # add the option of doing nothing else
        best_solution = max(best_solution, initial, key=lambda t: t.released_pressure)

    return best_solution


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    VALVES = [Valve.from_str(s) for s in PUZZLE_INPUT]
    VALVEI_BY_NAME = {v.name: 1 << i for i, v in enumerate(VALVES)}

    RATES = dict(v.to_rate(VALVEI_BY_NAME) for v in VALVES if v.rate)
    DISTANCES = get_shortest_paths(
        dict(v.to_connections(VALVEI_BY_NAME) for v in VALVES)
    )

    INITIAL = Token(VALVEI_BY_NAME["AA"], 0, 0, 30)

    pr = recursive_depthfirst(RATES, DISTANCES, INITIAL).released_pressure
    print(f"Part 1: {pr}")
