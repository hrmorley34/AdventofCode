from collections import Counter, defaultdict
from dataclasses import dataclass
from functools import lru_cache
from typing import Any

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


class Key(object):
    def __hash__(self) -> int:
        return id(self)


@dataclass
class InfiniMap:
    blocks: frozenset[tuple[int, int]]
    dim: tuple[int, int]
    pos_defs: defaultdict[frozenset[tuple[int, int]], Key]
    pos: dict[tuple[int, int], Key]
    known_mappings: dict[tuple[Key, tuple[Key, ...]], Key]

    @property
    def inv_pos_defs(self) -> dict[Key, frozenset[tuple[int, int]]]:
        return {v: k for k, v in self.pos_defs.items()}

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
        dd: defaultdict[frozenset[tuple[int, int]], Key] = defaultdict(Key)
        k0 = dd[frozenset(initial)]
        return cls(frozenset(blocks), (len(grid[0]), len(grid)), dd, {(0, 0): k0}, {})

    def step(self) -> Any:
        inv = self.inv_pos_defs
        empty = self.pos_defs[frozenset[tuple[int, int]]()]
        new_pages: defaultdict[tuple[int, int], set[tuple[int, int]]] = defaultdict(set)
        known_pages: dict[tuple[int, int], Key] = {}
        skips = partskips = 0
        for (gx, gy), k in self.pos.items():
            adjs = (k, tuple(self.pos.get((gx + dx, gy + dy), empty) for dx, dy in ADJ))
            mp = self.known_mappings.get(adjs)
            if mp is not None:
                known_pages[gx, gy] = mp
                partskips += 1
                if all(
                    (
                        self.pos.get((gx + d1x, gy + d1y), empty),
                        tuple(
                            self.pos.get((gx + d1x + d2x, gy + d1y + d2y), empty)
                            for d2x, d2y in ADJ
                        ),
                    )
                    in self.known_mappings
                    for d1x, d1y in ADJ
                ):
                    skips += 1
                    continue
            for (ax, ay), rxy in self.step_page(self.dim, self.blocks, inv[k]):
                new_pages[gx + ax, gy + ay].add(rxy)
        for k, v in new_pages.items():
            if k in known_pages:
                continue
            known_pages[k] = key = self.pos_defs[frozenset(v)]
            adjs = (
                self.pos.get(k, empty),
                tuple(self.pos.get((k[0] + dx, k[1] + dy), empty) for dx, dy in ADJ),
            )
            self.known_mappings[adjs] = key
        self.pos = known_pages
        return skips, partskips, len(new_pages)

    def count_reached(self) -> int:
        inv = self.inv_pos_defs
        return sum(len(inv[k]) * c for k, c in Counter(self.pos.values()).items())

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
    for i in range(STEPS2):
        if i % 10**6 == 0:
            print(f"{i:8}/{STEPS2}")
        m2.step()
    print(f"{STEPS2:8}/{STEPS2}")
    print("Part 2:", m2.count_reached())
