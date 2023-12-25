from __future__ import annotations

from collections import Counter
from itertools import chain

from puzzle_input import puzzle_input


class Wire:
    parts: tuple[str, str]

    def __init__(self, src: str, dest: str) -> None:
        a, b = tuple(sorted((src, dest)))
        self.parts = a, b

    @classmethod
    def from_line(cls, line: str) -> list[Wire]:
        left, rights = line.split(": ")
        return [cls(left, right) for right in rights.split()]

    def __repr__(self) -> str:
        return f"Wire<{self.parts[0]}, {self.parts[1]}>"

    def __contains__(self, component: str) -> bool:
        return component in self.parts

    def __hash__(self) -> int:
        return hash(self.parts)


class Wires:
    wires: set[Wire]

    def __init__(self, wires: set[Wire]) -> None:
        self.wires = wires

    @classmethod
    def from_lines(cls, lines: list[str]):
        return cls(set[Wire].union(*(set(Wire.from_line(line)) for line in lines)))

    def get_regions(self) -> list[set[str]]:
        wires = self.wires.copy()
        regions = [{*wires.pop().parts}]
        while wires:
            region_ext = {w for w in wires if any(p in regions[-1] for p in w.parts)}
            if not region_ext:
                regions.append({*wires.pop().parts})
            wires.difference_update(region_ext)
            regions[-1].update(*(w.parts for w in region_ext))
        return regions

    def try_cuts(self, wires: list[Wire] | None = None) -> int:
        if wires is None:
            wires = list(self.wires)
        for i1, c1 in enumerate(wires):
            for i2, c2 in enumerate(wires[i1 + 1 :], i1 + 1):
                for c3 in wires[i2 + 1 :]:
                    new_wires = Wires(self.wires - {c1, c2, c3})
                    regions = new_wires.get_regions()
                    if len(regions) == 2:
                        return len(regions[0]) * len(regions[1])
        assert False

    def find_all_paths(self) -> dict[tuple[str, str], list[Wire]]:
        reg = self.get_regions()
        assert len(reg) == 1

        d: dict[tuple[str, str], list[Wire]] = {}
        all_components = reg[0]
        all_pairs = {(c1, c2) for c1 in all_components for c2 in all_components}
        for c in all_components:
            d[c, c] = []
        for w in self.wires:
            d[w.parts] = d[w.parts[1], w.parts[0]] = [w]
        while diff := all_pairs.difference(d.keys()):
            passed = True
            old_d = d.copy()
            for (kv1, kv2), route1 in old_d.items():
                if kv1 not in {i[0] for i in diff}:
                    continue
                for (kv2a, kv3), route2 in old_d.items():
                    if kv2 == kv2a and (kv1, kv3) not in d:
                        d[kv1, kv3] = route1 + route2
                        d[kv3, kv1] = list(reversed(route1 + route2))
                        passed = False
            assert not passed  # nothing happened this iteration; give up
        return d

    def guess_cuts(self) -> list[Wire]:
        paths = self.find_all_paths()
        c = Counter(chain.from_iterable(paths.values()))
        return [w for w, _ in c.most_common(10)]


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    wires = Wires.from_lines(PUZZLE_INPUT)

    # Far too slow
    cuts = wires.guess_cuts()
    print(f"Found top {len(cuts)} suggestions")
    print("Part 1:", wires.try_cuts(cuts))
