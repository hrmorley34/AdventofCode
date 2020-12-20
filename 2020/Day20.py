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


class MegaTile(Tile):
    def __init__(self, lines: list[str]):
        self.lines = lines

    def __repr__(self):
        return "MegaTile(\n  {}\n)".format("\n  ".join(self.lines))

    def rotate_cw(self):
        lines2 = [str() for x in range(len(self.lines[0]))]
        for line in self.lines:
            for i, c in enumerate(line):
                lines2[i] = c + lines2[i]
        self.lines = lines2

    def rotate_ccw(self):
        lines2 = [str() for x in range(len(self.lines[0]))]
        for line in self.lines:
            for i, c in enumerate(line):
                lines2[9 - i] = lines2[9 - i] + c
        self.lines = lines2

    rotate_acw = rotate_ccw
    rotate = rotate_cw


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


def matchds(matches: _matches_type):
    matchd = defaultdict(dict)
    for t1, t2, t1r, t2r, b in matches:
        matchd[t1][t1r] = t2, t2r, b
        matchd[t2][t2r] = t1, t1r, b
    rmatchd = defaultdict(list)
    for t1, d in matchd.items():
        rmatchd[tuple(sorted(d.keys()))].append(t1)
    return matchd, rmatchd


def patch_together(tiles: dict[int, Tile]) -> MegaTile:
    matches = match_all_edges(tiles)
    matchd, rmatchd = matchds(matches)

    tile_order: list[int] = []
    queue = list(rmatchd[1, 2])[:1]  # start with top-left corner
    while len(queue):
        nexttile = queue.pop(0)
        neighbours = matchd[nexttile]
        if 1 in neighbours:
            n, nr, f = neighbours[1]
            if n not in queue:
                if nr == 0:
                    tiles[n].rotate_ccw()
                elif nr == 1:
                    tiles[n].rotate()
                    tiles[n].rotate()
                elif nr == 2:
                    tiles[n].rotate_cw()
                if f:
                    tiles[n].flipv()
                queue.insert(0, n)
        if 2 in neighbours:
            n, nr, f = neighbours[2]
            if n not in queue:
                if nr == 1:
                    tiles[n].rotate_ccw()
                elif nr == 2:
                    tiles[n].rotate()
                    tiles[n].rotate()
                elif nr == 3:
                    tiles[n].rotate_cw()
                if f:
                    tiles[n].fliph()
                queue.append(n)
        tile_order.append(nexttile)

        # recalculate edges
        matches = match_all_edges(tiles)
        matchd, rmatchd = matchds(matches)

    tile_map: list[list[Tile]] = []
    width = round(len(tiles) ** 0.5)
    counter = 0
    for t in tile_order:
        if counter % width == 0:
            tile_map.append([])
        tile_map[-1].append(tiles[t])
        counter += 1

    tile_text: list[str] = []
    for row in tile_map:
        for x in range(8):
            tile_text.append("")
        for tile in row:
            for i, line in enumerate(tile.lines[1:-1], -8):
                tile_text[i] += line[1:-1]

    return MegaTile(tile_text)


SEA_MONSTER = [
    "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   ",
]


def calc_roughness(mega: MegaTile):
    for r in range(16):
        found = False
        for x in range(0, len(mega.lines[0]) - len(SEA_MONSTER[0])):
            for y in range(0, len(mega.lines) - len(SEA_MONSTER)):
                rowcheck = True
                for megaline, line in zip(mega.lines[y:], SEA_MONSTER):
                    for lc, mc in zip(megaline[x:], line):
                        if mc == "#" and lc != "#":
                            rowcheck = False
                            break
                if rowcheck:
                    found = True
                    break
            if found:
                break
        if found:
            break

        if r % 8 == 0:
            mega.flipv()
        elif r % 2 == 1:
            mega.rotate()
        else:
            mega.fliph()

    if found:
        positions = []
        for x in range(0, len(mega.lines[0]) - len(SEA_MONSTER[0])):
            for y in range(0, len(mega.lines) - len(SEA_MONSTER)):
                rowcheck = True
                for megaline, line in zip(mega.lines[y:], SEA_MONSTER):
                    for lc, mc in zip(megaline[x:], line):
                        if mc == "#" and lc != "#":
                            rowcheck = False
                            break
                if rowcheck:
                    positions.append((x, y))

        for x, y in positions:
            for yi, line in enumerate(SEA_MONSTER, y):
                for xi, mc in enumerate(line, x):
                    if mc == "#":
                        mega.lines[yi] = (
                            mega.lines[yi][:xi] + "O" + mega.lines[yi][xi + 1 :]
                        )

        return sum([r.count("#") for r in mega.lines])


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().split("\n\n")
    # PUZZLE_INPUT = "\n".join(input().splitlines()).split("\n\n")  # for hydrogen

    tiles = {t.id: t for t in (Tile(text) for text in PUZZLE_INPUT)}
    matches = match_all_edges(tiles)

    m = 1
    corners = [k for k, v in count_connections(matches).items() if v == 2]
    print(f"{len(corners)} corners")
    for p in corners:
        m *= p
    print(m)

    mega = patch_together(tiles)

    print(calc_roughness(mega))
