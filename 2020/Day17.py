from puzzle_input import puzzle_input
from collections import defaultdict
import numpy as np


ADJS = {(x, y, z) for x in range(-1, 2) for y in range(-1, 2) for z in range(-1, 2)}
ADJS.remove((0, 0, 0))
ADJS4 = {
    (w, x, y, z)
    for w in range(-1, 2)
    for x in range(-1, 2)
    for y in range(-1, 2)
    for z in range(-1, 2)
}
ADJS4.remove((0, 0, 0, 0))


class MapWrapper:
    "(the 3D one)"
    obj: defaultdict[int, defaultdict[int, defaultdict[int, bool]]]

    def __init__(self, state2d: list[str]):
        self.obj = defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: False)))
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
        if isinstance(k, (tuple, np.ndarray)):
            o = self.obj
            for key in reversed(k):
                o = o[key]
            return o
        return self.obj[k]

    def __setitem__(self, k, v):
        if isinstance(k, (tuple, np.ndarray)):
            o = self.obj
            for key in reversed(k[1:]):
                o = o[key]
            o[k[0]] = v
        else:
            raise NotImplementedError

    def __delitem__(self, k):
        if isinstance(k, (tuple, np.ndarray)):
            o = self.obj
            for key in reversed(k[1:]):
                o = o[key]
            del o[k[0]]
        else:
            del self.obj[k]

    def clean(self):
        for k0, layer0 in list(self.obj.items()):
            for k1, layer1 in list(layer0.items()):
                for k2, layer2 in list(layer1.items()):
                    if not layer2:
                        del layer1[k2]
                if not layer1:
                    del layer0[k1]
            if not layer0:
                del self.obj[k0]

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
                    # for a in ADJS:
                    #     p = np.array((x, y, z)) + np.array(a)
                    #     if not self[p]:
                    #         del self[p]
                    #     if not self[p[-2:]]:
                    #         del self[p[-2:]]
                    #     if not self[p[-1]]:
                    #         del self[p[-1]]
        for item, value in overwrite:
            self[item] = value
        self.clean()

    def count_neighbours(self, xyz: tuple[int, int, int]) -> int:
        return [self[np.array(xyz) + np.array(a)] for a in ADJS].count(True)

    @property
    def active_count(self):
        total = 0
        for layer in self.obj.values():
            for row in layer.values():
                total += sum(map(int, map(bool, row.values())))
        return total


class MapWrapper4:
    "now with +33% more dimensions"
    obj: defaultdict[int, defaultdict[int, defaultdict[int, defaultdict[int, bool]]]]

    def __init__(self, state2d: list[str]):
        self.obj = defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: False))))
        for y, line in enumerate(state2d):
            for x, char in enumerate(line):
                self.obj[0][0][y][x] = True if char == "#" else False

    def __repr__(self):
        return "MapWrapper4D({}x{}x{}x{})".format(
            *[abs(ma - mi) + 1 for mi, ma in self.dimensions]
        )

    def __getitem__(self, k):
        if isinstance(k, (tuple, np.ndarray)):
            o = self.obj
            for key in reversed(k):
                o = o[key]
            return o
        return self.obj[k]

    def __setitem__(self, k, v):
        if isinstance(k, (tuple, np.ndarray)):
            o = self.obj
            for key in reversed(k[1:]):
                o = o[key]
            o[k[0]] = v
        else:
            raise NotImplementedError

    def __delitem__(self, k):
        if isinstance(k, (tuple, np.ndarray)):
            o = self.obj
            for key in reversed(k[1:]):
                o = o[key]
            del o[k[0]]
        else:
            del self.obj[k]

    def clean(self):
        for k0, layer0 in list(self.obj.items()):
            for k1, layer1 in list(layer0.items()):
                for k2, layer2 in list(layer1.items()):
                    for k3, layer3 in list(layer2.items()):
                        if not layer3:
                            del layer2[k3]
                    if not layer2:
                        del layer1[k2]
                if not layer1:
                    del layer0[k1]
            if not layer0:
                del self.obj[k0]

    @property
    def dimensions(self):
        dimensions = [[0, 0], [0, 0], [0, 0], (min(self.obj.keys()), max(self.obj.keys()))]
        for layer in self.obj.values():
            for k, layer2 in layer.items():
                if k < dimensions[2][0]:
                    dimensions[2][0] = k
                if k > dimensions[2][1]:
                    dimensions[2][1] = k
                for k2, layer3 in layer2.items():
                    if k2 < dimensions[1][0]:
                        dimensions[1][0] = k2
                    if k2 > dimensions[1][1]:
                        dimensions[1][1] = k2
                    for k3, row in layer3.items():
                        if k3 < dimensions[0][0]:
                            dimensions[0][0] = k3
                        if k3 > dimensions[0][1]:
                            dimensions[0][1] = k3
        return tuple(map(tuple, dimensions))

    def update(self):
        overwrite = []
        (minx, maxx), (miny, maxy), (minz, maxz), (minw, maxw) = self.dimensions
        for w in range(minw - 1, maxw + 2):
            for z in range(minz - 1, maxz + 2):
                for y in range(miny - 1, maxy + 2):
                    for x in range(minx - 1, maxx + 2):
                        c = self[x, y, z, w]
                        n = self.count_neighbours((x, y, z, w))
                        if c and n not in (2, 3):
                            overwrite.append(((x, y, z, w), False))
                        elif not c and n == 3:
                            overwrite.append(((x, y, z, w), True))
                        # for a in ADJS:
                        #     p = np.array((x, y, z)) + np.array(a)
                        #     if not self[p]:
                        #         del self[p]
                        #     if not self[p[-2:]]:
                        #         del self[p[-2:]]
                        #     if not self[p[-1]]:
                        #         del self[p[-1]]
        for item, value in overwrite:
            self[item] = value
        self.clean()

    def count_neighbours(self, xyzw: tuple[int, int, int, int]) -> int:
        return [self[np.array(xyzw) + np.array(a)] for a in ADJS4].count(True)

    @property
    def active_count(self):
        total = 0
        for layer in self.obj.values():
            for layer2 in layer.values():
                for row in layer2.values():
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

MAP4 = MapWrapper4(PUZZLE_INPUT.splitlines())
print(repr(MAP4))
for x in range(6):
    MAP4.update()
    print(MAP4.dimensions)
print(MAP4.active_count)
del MAP4
