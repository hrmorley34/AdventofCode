from puzzle_input import puzzle_input
from collections import defaultdict, Counter


class Tile:
    def __init__(self, text: str):
        id, *lines = text.splitlines()
        self.id = int(id[5:-1])
        self.lines = lines[:10]

    def __repr__(self):
        return "Tile(\n  {}\n)".format("\n  ".join(self.lines))

    def rotate_cw(self):
        lines2 = [str() for x in range(10)]
        for line in self.lines:
            for i, c in enumerate(line):
                lines2[i] = c + lines2[i]
        self.lines = lines2

    def rotate_ccw(self):
        lines2 = [str() for x in range(10)]
        for line in self.lines:
            for i, c in enumerate(line):
                lines2[9 - i] = lines2[9 - i] + c
        self.lines = lines2

    rotate_acw = rotate_ccw
    rotate = rotate_cw

    def fliph(self):
        self.lines = ["".join(reversed(line)) for line in self.lines]

    def flipv(self):
        self.lines = list(reversed(self.lines))

    flip = flipv

    @property
    def edges(self):
        return (
            self.lines[0],
            "".join(line[-1] for line in self.lines),
            "".join(reversed(self.lines[-1])),
            "".join(line[0] for line in reversed(self.lines)),
        )


def match_flipped(e0: str, e1: str):
    if len(e0) != 10 or len(e1) != 10:
        return False
    return e0 == "".join(reversed(e1))


_matches_type = set[tuple[int, int], tuple[int, int], bool]


def match_all_edges(tiles: dict[int, Tile]) -> _matches_type:
    matches = set()
    tlist = list(sorted(tiles.items()))
    for i, (t1i, t1) in enumerate(tlist):
        for t2i, t2 in tlist[i + 1 :]:
            for e1, edge1 in enumerate(t1.edges):
                for e2, edge2 in enumerate(t2.edges):
                    if match_flipped(edge1, edge2):
                        matches.add((t1i, t2i, e1, e2, False))
                    elif edge1 == edge2:
                        matches.add((t1i, t2i, e1, e2, True))
    return matches


def count_connections(matches: _matches_type) -> list[int]:
    counts = Counter()
    for t1, t2, t1r, t2r, b in matches:
        counts[t1] += 1
        counts[t2] += 1
    return counts


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().split("\n\n")

    tiles = {t.id: t for t in (Tile(text) for text in PUZZLE_INPUT)}
    matches = match_all_edges(tiles)

    m = 1
    corners = [k for k, v in count_connections(matches).items() if v == 2]
    print(f"{len(corners)} corners")
    for p in corners:
        m *= p
    print(m)
