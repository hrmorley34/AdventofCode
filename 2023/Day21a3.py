from dataclasses import dataclass
from functools import lru_cache
from typing import Generator

from puzzle_input import puzzle_input

ADJ = [(-1, 0), (0, -1), (1, 0), (0, 1)]


@dataclass(frozen=True)
class Map:
    blocks: frozenset[tuple[int, int]]
    dim: tuple[int, int]
    initial: tuple[int, int]

    @classmethod
    def from_map(cls, grid: list[str]):
        assert len(grid) % 2 == 1
        assert len(grid[0]) % 2 == 1
        assert len(grid) == len(grid[0])

        blocks: set[tuple[int, int]] = set()
        initial: list[tuple[int, int]] = []
        for y, line in enumerate(grid):
            for x, c in enumerate(line):
                if c == "#":
                    blocks.add((x, y))
                elif c == "S":
                    initial.append((x, y))
                else:
                    assert c == "."
        assert len(initial) == 1
        return cls(frozenset(blocks), (len(grid[0]), len(grid)), initial[0])

    def steps_from(
        self, src: tuple[int, int]
    ) -> Generator[tuple[int, tuple[int, int]], None, None]:
        seen: set[tuple[int, int]] = set(self.blocks)  # auto-ignore blocks
        q = [src]
        new_q: list[tuple[int, int]] = []
        steps = 0
        while q:
            for ix, iy in q:
                if (ix, iy) in seen:
                    continue
                yield (steps, (ix, iy))
                seen.add((ix, iy))
                for dx, dy in ADJ:
                    px, py = ix + dx, iy + dy
                    if px < 0 or px >= self.dim[0] or py < 0 or py >= self.dim[1]:
                        continue
                    elif (px, py) in seen:
                        continue
                    new_q.append((px, py))
            q, new_q = new_q, []
            steps += 1

    def steps_between(self, src: tuple[int, int], dest: tuple[int, int]) -> int:
        for steps, pos in self.steps_from(src):
            if pos == dest:
                return steps
        raise ValueError

    @lru_cache
    def times_to_access(self, src: tuple[int, int]) -> dict[tuple[int, int], int]:
        return {(x, y): s for s, (x, y) in self.steps_from(src)}

    def part_1(self, STEPS: int) -> int:
        return sum(
            t <= STEPS and (t - STEPS) % 2 == 0
            for t in self.times_to_access(self.initial).values()
        )

    def times_to_access_distant(self, STEPS: int) -> int:
        tta_centre = self.times_to_access(self.initial)

        tta_tl = self.times_to_access((0, 0))
        tta_tr = self.times_to_access((self.dim[0] - 1, 0))
        tta_bl = self.times_to_access((0, self.dim[1] - 1))
        tta_br = self.times_to_access((self.dim[0] - 1, self.dim[1] - 1))

        tta_tl_even = {k for k, v in tta_tl.items() if v % 2 == 0}
        tta_tl_odd = {k for k, v in tta_tl.items() if v % 2 == 1}
        tta_full = (
            {0: tta_tl_even, 1: tta_tl_odd}
            if self.initial in tta_tl_even
            else {0: tta_tl_odd, 1: tta_tl_even}
        )
        assert self.initial in tta_full[0]
        assert self.initial not in tta_full[1]

        total = 0
        for x in range(-(STEPS // self.dim[0]) - 1, STEPS // self.dim[0] + 2):
            # if x % 10**5 == 0:
            #     print(x)
            for y in range(-(STEPS // self.dim[1]) - 1, STEPS // self.dim[1] + 2):
                corners: list[tuple[tuple[int, int], dict[tuple[int, int], int]]] = []
                if x <= 0 and y <= 0:
                    corners.append(((0, 0), tta_br))
                if x >= 0 and y <= 0:
                    corners.append(((self.dim[0] - 1, 0), tta_bl))
                if x <= 0 and y >= 0:
                    corners.append(((0, self.dim[1] - 1), tta_tr))
                if x >= 0 and y >= 0:
                    corners.append(((self.dim[0] - 1, self.dim[1] - 1), tta_tl))
                assert len(corners) in (1, 2, 4)
                fill = False
                non_fill = False
                for corner, _ in corners:
                    t_t_corner = tta_centre[corner]

                    dist = (
                        t_t_corner
                        + max((abs(x) - 1) * self.dim[0] + 1, 0)
                        + max((abs(y) - 1) * self.dim[1] + 1, 0)
                    )
                    dist_next = t_t_corner + abs(x) * self.dim[0] + abs(y) * self.dim[1]
                    # print((x, y), STEPS, dist_next, dist)
                    if STEPS > dist_next:
                        fill = True
                    elif STEPS > dist:
                        non_fill = True
                    else:
                        continue

                if fill:
                    # print("FILL", (x, y))
                    # filled completely
                    total += len(tta_full[(x + y) % 2])
                elif non_fill:
                    # print("NON-FILL", (x, y))
                    accessible: set[tuple[int, int]] = set()
                    for corner, tta in corners:
                        t_t_corner = tta_centre[corner]
                        dist = (
                            t_t_corner
                            + max((abs(x) - 1) * self.dim[0] + 1, 0)
                            + max((abs(y) - 1) * self.dim[1] + 1, 0)
                        )
                        for pos, d in tta.items():
                            if dist + d <= STEPS and (dist + d - STEPS) % 2 == 0:
                                accessible.add(pos)
                    total += len(accessible)
                # else:
                #     print("EMPTY", (x, y))
        return total + 1  # I don't know why there's a +1 here


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    m = Map.from_map(PUZZLE_INPUT)

    T1 = 64
    print("Part 1:", m.part_1(T1))

    assert len(PUZZLE_INPUT) == len(PUZZLE_INPUT[0])

    T2 = 26501365
    # still too slow
    # print("Part 2:", m.times_to_access_distant(T2))

    vs = {
        i: m.times_to_access_distant((T2 % len(PUZZLE_INPUT)) + (len(PUZZLE_INPUT) * i))
        for i in range(3)
    }
    c = vs[0]
    a, d = divmod(vs[2] - 2 * vs[1] + vs[0], 2)
    assert d == 0
    b = vs[1] - a - c

    n = T2 // len(PUZZLE_INPUT)
    print("Part 2:", a * n**2 + b * n + c)
