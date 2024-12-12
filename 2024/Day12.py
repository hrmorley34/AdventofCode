from dataclasses import dataclass

from puzzle_input import puzzle_input

NEIGHBOURS = [(1, 0), (0, 1), (-1, 0), (0, -1)]


@dataclass
class Region:
    name: str
    coords: set[tuple[int, int]]

    def get_perimiter(self) -> int:
        return sum(
            (x + dx, y + dy) not in self.coords
            for x, y in self.coords
            for dx, dy in NEIGHBOURS
        )

    def get_sides(self) -> tuple[int, int]:
        edges: set[tuple[tuple[int, int], tuple[int, int]]] = set()
        sides = 0
        for x, y in sorted(self.coords):
            for dx, dy in NEIGHBOURS:
                if (x + dx, y + dy) not in self.coords:
                    edges.add(((x, y), (dx, dy)))
                    if ((x - (not dx), y - (not dy)), (dx, dy)) not in edges:
                        # this is not a continuation of an edge
                        sides += 1
        return len(edges), sides

    def get_prices(self) -> tuple[int, int]:
        p, s = self.get_sides()
        return len(self.coords) * p, len(self.coords) * s


def explore_region(m: list[str], r: Region) -> Region:
    to_explore: set[tuple[int, int]] = r.coords.copy()
    while to_explore:
        x, y = to_explore.pop()
        for dx, dy in NEIGHBOURS:
            cx, cy = c = x + dx, y + dy
            if not (0 <= cy < len(m) and 0 <= cx < len(m[cy])):
                continue
            elif c in r.coords:
                continue
            elif m[cy][cx] == r.name:
                r.coords.add(c)
                to_explore.add(c)
    return r


def explore_regions(m: list[str]) -> list[Region]:
    regions: list[Region] = []
    for y in range(len(m)):
        for x in range(len(m[y])):
            if any((x, y) in r.coords for r in regions):
                continue
            r = Region(m[y][x], {(x, y)})
            regions.append(explore_region(m, r))
    assert sum(len(r.coords) for r in regions) == len(m) * len(m[0])
    return regions


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    REGIONS = explore_regions(PUZZLE_INPUT)

    s1 = s2 = 0
    for r in REGIONS:
        p1, p2 = r.get_prices()
        s1 += p1
        s2 += p2
    print("Part 1:", s1)
    print("Part 2:", s2)
