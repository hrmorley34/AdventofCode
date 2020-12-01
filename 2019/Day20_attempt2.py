import string, time
from puzzle_input import puzzle_input

# A fresh attempt

DIRECTIONS = [1j, 1, -1j, -1]


class Map:
    _map: list
    _nodes: set
    portals: dict
    nodes: dict

    def __init__(self, map_text: str):
        self._map = list(map(str, str(map_text).splitlines()))
        self._nodes = None

    def __getitem__(self, k):
        if isinstance(k, complex):
            assert 0 <= k.imag < self.dimensions[1]
            assert 0 <= k.real < self.dimensions[0]
            return self._map[int(k.imag)][int(k.real)]
        elif isinstance(k, (tuple, list)):
            assert 0 <= k[1] < self.dimensions[1]
            assert 0 <= k[0] < self.dimensions[0]
            return self._map[int(k[1])][int(k[0])]
        else:
            return self._map[int(k)]

    @property
    def dimensions(self):
        return (len(self._map[0]), len(self._map))

    def find_nodes(self, corners: bool = True):
        nodes = set()
        portals = {}
        for y in range(1, self.dimensions[1] - 1):
            for x in range(1, self.dimensions[0] - 1):
                if self[x, y] == ".":
                    N, E, S, W = [
                        c == "." for c in neighbours(self, complex(x, y)).values()
                    ]
                    count = sum((N, E, S, W))
                    if count >= 3:
                        # plus or T-junction; always count
                        nodes.add(complex(x, y))
                    elif (N and S) or (E and W):
                        # Straight path; don't bother counting
                        continue
                    elif corners and count == 2:
                        # Must be corner now (straight has been skipped)
                        nodes.add(complex(x, y))
                    elif count == 1:
                        # Dead end
                        nodes.add(complex(x, y))
                elif self[x, y] in string.ascii_letters:
                    try:
                        c = [self[complex(x, y) + d] for d in DIRECTIONS]
                    except AssertionError:
                        continue
                    if "." in c:
                        nodes.add(complex(x, y))
                        portals[find_portal_name(complex(x, y), self)] = complex(x, y)
        self._nodes = nodes
        self.portals = portals
        return nodes, portals

    def get_node_map(self):
        nodes, portals = self.find_nodes(corners=True)
        invportals = {name: pos for pos, name in portals.items()}
        new_nodes = {}
        for n in nodes:
            new_nodes[n] = Node(self, n)
        for p, n in new_nodes.items():
            for d, t in neighbours(self, p).items():
                if t == "." or (
                    t in string.ascii_letters and self[p] not in string.ascii_letters
                ):
                    checkpos = p + d
                    distance = 1
                    while checkpos not in nodes:
                        checkpos += d
                        distance += 1
                        if self[checkpos] != ".":
                            break
                    if self[checkpos] not in "." + string.ascii_letters:
                        continue
                    n.neighbours += ((new_nodes[checkpos], distance),)
            if p in portals.values():
                name = invportals[p]
                if name in ("AA", "ZZ"):
                    continue
                elif name[0] in string.ascii_uppercase:
                    other = portals[name.lower()]
                else:
                    other = portals[name.upper()]
                # -1 makes the portals count properly (1 rather than 2/3)
                n.neighbours += ((new_nodes[other], -1),)

        self.nodes = new_nodes
        return self.nodes


def neighbours(map: Map, position: complex) -> dict:
    return {d: map[position + d] for d in DIRECTIONS}


def find_portal_name(position: complex, map_: Map):
    dimensions = complex(*map_.dimensions)

    # Capital outer, lowercase inner
    def casify(t):
        if (
            position.real < 3
            or position.real > dimensions.real - 3
            or position.imag < 3
            or position.imag > dimensions.imag - 3
        ):
            return t.upper()
        return t.lower()

    assert map_[position] in string.ascii_letters
    for p in DIRECTIONS:
        if map_[position + p] in string.ascii_letters:
            if p in (1, 1j):
                return casify(map_[position] + map_[position + p])
            else:
                return casify(map_[position + p] + map_[position])


class Node:
    map: Map
    coords: complex
    neighbours: tuple  # (node, distance)

    def __init__(self, map_: Map, coords: complex, neighbours: tuple = None):
        if isinstance(coords, (tuple, list)):
            coords = complex(coords[0], coords[1])

        self.map = map_
        self.coords = coords
        self.neighbours = neighbours or tuple()

    def __eq__(self, node):
        return self.coords == node.coords

    def __repr__(self):
        return "Node({}, {})".format(int(self.coords.real), int(self.coords.imag))


def dijkstra(start, end):
    queue = []  # (node, distance, via)
    done = {}  # node_pos: (node, distance, via)

    # -2 makes the start/end portals count properly
    current = (start, -2, None)
    while current[0] != end:
        # print(current)
        for n, d in current[0].neighbours:
            dist = current[1] + d
            if n.coords in done:
                continue
            else:
                queue.append((n, dist, current[0]))
        done[current[0].coords] = current
        queue = sorted(queue, key=lambda t: t[1])
        current = queue.pop(0)

    return current[1]


def depth_dijkstra(start, end):
    queue = []  # (depth, node, distance, via)
    done = {}  # (depth, node_pos): (depth, node, distance, via)
    revportal = {pos: name for name, pos in start.map.portals.items()}

    # -2 makes the start/end portals count properly
    current = (0, start, -2, None)
    while current[0] != 0 or current[1] != end:
        for n, d in current[1].neighbours:
            dist = current[2] + d
            dep = current[0]
            if current[1].coords in revportal and n.coords in revportal:
                if revportal[n.coords][0] in string.ascii_lowercase:
                    dep -= 1
                else:
                    dep += 1
            if dep < 0:
                # No recursing beyond top level 0
                continue
            if (dep, n.coords) in done:
                continue
            else:
                queue.append((dep, n, dist, current[0:2]))
        done[(current[0], current[1].coords)] = current
        queue = sorted(queue, key=lambda t: t[2])
        current = queue.pop(0)

    #check = current
    #while check[3][1] != start:
    #    if check[1].coords in revportal:
    #        print(check[0], revportal[check[1].coords], check[2])
    #    check = done[check[3][0], check[3][1].coords]
    return current[2]


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()

    map = Map(PUZZLE_INPUT)

    m = map.get_node_map()
    start = m[map.portals["AA"]]
    end = m[map.portals["ZZ"]]

    print(dijkstra(start, end))
    print(depth_dijkstra(start, end))
