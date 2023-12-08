import re
from dataclasses import dataclass
from itertools import cycle
from math import lcm
from typing import Callable, Iterable

from puzzle_input import puzzle_input

START = "AAA"
END = "ZZZ"
RE_LINE = re.compile(r"(\w{3}) = \((\w{3}), (\w{3})\)")


@dataclass
class Map:
    pairs: dict[str, tuple[str, str]]

    @classmethod
    def from_lines(cls, lines: list[str]):
        d: dict[str, tuple[str, str]] = {}
        for m in map(RE_LINE.match, lines):
            assert m is not None
            d[m[1]] = (m[2], m[3])
        return cls(d)

    def follow_rl(self, pos: str, rl: str) -> str:
        pair = self.pairs[pos]
        if rl == "L":
            return pair[0]
        elif rl == "R":
            return pair[1]
        else:
            raise ValueError(rl)

    def follow_path(
        self, start: str, end: str | Callable[[str], bool], path: Iterable[str]
    ) -> int:
        if not callable(end):
            end_v = end
            end = lambda p: p == end_v  # noqa: E731
        steps = 0
        path = cycle(path)
        pos = start
        while not end(pos):
            steps += 1
            pos = self.follow_rl(pos, next(path))
        return steps


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    path, _, *pairs = PUZZLE_INPUT

    m = Map.from_lines(pairs)
    steps = m.follow_path(START, END, path)
    print("Part 1:", steps)

    p2_ends: dict[str, int] = {START: steps}
    keys = [k for k in m.pairs if k[-1] == "A" and k != START]
    for k in keys:
        p2_ends[k] = m.follow_path(k, lambda p: p[-1] == "Z", path)
    print("Part 2:", lcm(*p2_ends.values()))
