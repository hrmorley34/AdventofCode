from collections import defaultdict
from dataclasses import dataclass
from functools import lru_cache

from puzzle_input import puzzle_input

ADJ = [(-1, 0), (0, -1), (1, 0), (0, 1)]


@dataclass
class Map:
    blocks: set[tuple[int, int]]
    dim: tuple[int, int]
    pos: set[tuple[int, int]]

    @classmethod
    def from_map(cls, grid: list[str]):
        blocks: set[tuple[int, int]] = set()
        initial: set[tuple[int, int]] = set()
        for y, line in enumerate(grid):
            for x, c in enumerate(line):
                if c == "#":
                    blocks.add((x, y))
                elif c == "S":
                    initial.add((x, y))
                else:
                    assert c == "."
        return cls(blocks, (len(grid[0]), len(grid)), initial)

    def step(self) -> None:
        new_pos: set[tuple[int, int]] = set()
        for px, py in self.pos:
            for dx, dy in ADJ:
                x, y = px + dx, py + dy
                if (x, y) in self.blocks:
                    continue
                elif x < 0 or x >= self.dim[0] or y < 0 or y >= self.dim[1]:
                    print("WARNING: Out of range")
                    continue
                else:
                    new_pos.add((x, y))
        self.pos = new_pos


@dataclass
class InfiniMap:
    blocks: frozenset[tuple[int, int]]
    dim: tuple[int, int]
    pos: dict[tuple[int, int], frozenset[tuple[int, int]]]

    @classmethod
    def from_map(cls, grid: list[str]):
        blocks: set[tuple[int, int]] = set()
        initial: defaultdict[tuple[int, int], set[tuple[int, int]]] = defaultdict(set)
        for y, line in enumerate(grid):
            for x, c in enumerate(line):
                if c == "#":
                    blocks.add((x, y))
                elif c == "S":
                    initial[(0, 0)].add((x, y))
                else:
                    assert c == "."
        return cls(
            frozenset(blocks),
            (len(grid[0]), len(grid)),
            {k: frozenset(v) for k, v in initial.items()},
        )

    def step(self) -> None:
        new_pages: defaultdict[tuple[int, int], set[tuple[int, int]]] = defaultdict(set)
        for (gx, gy), mxys in self.pos.items():
            for (ax, ay), rxy in self.step_page(self.dim, self.blocks, mxys):
                new_pages[gx + ax, gy + ay].add(rxy)
        self.pos = {k: frozenset(v) for k, v in new_pages.items()}

    def count_reached(self) -> int:
        return sum(map(len, self.pos.values()))

    @staticmethod
    @lru_cache
    def is_full(
        dim: tuple[int, int],
        blocks: frozenset[tuple[int, int]],
        pos: frozenset[tuple[int, int]],
    ) -> bool:
        t = next(iter(pos))
        m = (t[0] + t[1]) % 2
        for x in range(dim[0]):
            for y in range(((x + m) % 2), dim[1], 2):
                assert (x + y) % 2 == m
                if (x, y) not in blocks and (x, y) not in pos:
                    return False
        return True

    @staticmethod
    @lru_cache
    def step_page(
        dim: tuple[int, int],
        blocks: frozenset[tuple[int, int]],
        pos: frozenset[tuple[int, int]],
    ) -> set[tuple[tuple[int, int], tuple[int, int]]]:
        new_pos: set[tuple[tuple[int, int], tuple[int, int]]] = set()
        for px, py in pos:
            for dx, dy in ADJ:
                x, y = px + dx, py + dy
                (gx, mx), (gy, my) = divmod(x, dim[0]), divmod(y, dim[1])
                if (mx, my) in blocks:
                    continue
                else:
                    new_pos.add(((gx, gy), (mx, my)))
        return new_pos


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    m = Map.from_map(PUZZLE_INPUT)

    for _ in range(64):
        m.step()
    print("Part 1:", len(m.pos))

    m2 = InfiniMap.from_map(PUZZLE_INPUT)
    STEPS2 = 26501365

    for _ in range(STEPS2 % len(PUZZLE_INPUT)):
        m2.step()
    vs = {0: m2.count_reached()}
    print(0)
    for i in range(1, 3):
        for _ in range(len(PUZZLE_INPUT)):
            m2.step()
        vs[i] = m2.count_reached()
        print(i)

    c = vs[0]
    a, d = divmod(vs[2] - 2 * vs[1] + vs[0], 2)
    assert d == 0
    b = vs[1] - a - c

    n = STEPS2 // len(PUZZLE_INPUT)
    print("Part 2:", a * n**2 + b * n + c)
