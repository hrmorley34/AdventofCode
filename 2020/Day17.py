from puzzle_input import puzzle_input
from collections import defaultdict
import numpy


ADJS = {(x, y, z) for x in range(-1, 2) for y in range(-1, 2) for z in range(-1, 2)}
ADJS.remove((0, 0, 0))


class MapWrapper:
    "(the 3D one)"
    obj: dict

    def __init__(self, state2d: list[str]):
        self.obj = defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: None)))
        for y, line in enumerate(state2d):
            for x, char in enumerate(line):
                self.obj[0][y][x] = True if char == "#" else False

    def __repr__(self):
        layertexts = []
        for i, layer in sorted(self.obj.items()):
            rowtexts = []
            for ri, r in sorted(layer.items()):
                rowtexts.append(
                    "".join("#" if c else "." for ci, c in sorted(r.items()))
                )
            layertexts.append("z={}\n  {}".format(i, "\n  ".join(rowtexts)))
        s = "MapWrapper(\n{}\n)".format("\n\n".join(layertexts))
        return s

    def __getitem__(self, k):
        if isinstance(k, (tuple, numpy.ndarray)):
            o = self.obj
            for key in reversed(k):
                o = o[key]
            return o
        return self.obj[k]

    def __setitem__(self, k, v):
        if isinstance(k, (tuple, numpy.ndarray)):
            o = self.obj
            for key in reversed(k[1:]):
                o = o[key]
            o[k[0]] = v
        else:
            raise NotImplementedError

    def __delitem__(self, k):
        if isinstance(k, (tuple, numpy.ndarray)):
            o = self.obj
            for key in reversed(k[1:]):
                o = o[key]
            del o[k[0]]
        else:
            del self.obj[k]

    @property
    def dimensions(self):
        dimensions = [[0, 0], [0, 0], (min(self.obj.keys()), max(self.obj.keys()))]
        for layer in self.obj.values():
            for k, row in layer.items():
                if k < dimensions[1][0]:
                    dimensions[1][0] = k
                if k > dimensions[1][1]:
                    dimensions[1][1] = k
                for k2 in row:
                    if k2 < dimensions[0][0]:
                        dimensions[0][0] = k2
                    if k2 > dimensions[0][1]:
                        dimensions[0][1] = k2
        return tuple(map(tuple, dimensions))

    def update(self):
        overwrite = []
        (minx, maxx), (miny, maxy), (minz, maxz) = self.dimensions
        for z in range(minz - 1, maxz + 2):
            for y in range(miny - 1, maxy + 2):
                for x in range(minx - 1, maxx + 2):
                    c = self[x, y, z]
                    n = self.count_neighbours((x, y, z))
                    if c and n not in (2, 3):
                        overwrite.append(((x, y, z), False))
                    elif not c and n == 3:
                        overwrite.append(((x, y, z), True))
                    for a in ADJS:
                        p = numpy.array((x, y, z)) + numpy.array(a)
                        if self[p] is None:
                            del self[p]
                        if not self[p[-2:]]:
                            del self[p[-2:]]
                        if not self[p[-1]]:
                            del self[p[-1]]
        for item, value in overwrite:
            self[item] = value

    def count_neighbours(self, xyz: tuple[int, int, int]) -> int:
        return [self[numpy.array(xyz) + numpy.array(a)] for a in ADJS].count(True)

    @property
    def active_count(self):
        total = 0
        for layer in self.obj.values():
            for row in layer.values():
                total += sum(map(int, map(bool, row.values())))
        return total


PUZZLE_INPUT = puzzle_input()

MAP = MapWrapper(PUZZLE_INPUT.splitlines())
print(repr(MAP))
for x in range(6):
    MAP.update()
    print(MAP.dimensions)
print(MAP.active_count)
del MAP
