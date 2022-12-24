import heapq
from typing import Any, NamedTuple

from puzzle_input import puzzle_input


class Position(NamedTuple):
    x: int
    y: int

    def __add__(self, pos: Any) -> "Position":
        if isinstance(pos, Position):
            return Position(self.x + pos.x, self.y + pos.y)
        return NotImplemented

    def __neg__(self) -> "Position":
        return Position(-self.x, -self.y)

    def __sub__(self, pos: Any) -> "Position":
        if isinstance(pos, Position):
            return self + (-pos)
        return NotImplemented

    @property
    def length(self) -> float:
        return (self.x**2 + self.y**2) ** 0.5


ARROW_DIRECTIONS = {
    ">": Position(1, 0),
    "v": Position(0, 1),
    "<": Position(-1, 0),
    "^": Position(0, -1),
}
REVERSE_ARROW_DIRECTIONS = {v: k for k, v in ARROW_DIRECTIONS.items()}


class Blizzard(NamedTuple):
    pos: Position
    dir: Position

    def update_position(self, size: tuple[int, int]) -> "Blizzard":
        newpos = self.pos + self.dir
        newpos = Position(newpos.x % size[0], newpos.y % size[1])
        return Blizzard(newpos, self.dir)


class Valley(NamedTuple):
    start: Position
    end: Position
    size: tuple[int, int]  # of space, not of walls

    steps: int
    blizzards: frozenset[Blizzard]
    occupied: frozenset[Position]

    @classmethod
    def parse_lines(cls, s: str):
        lines = s.splitlines()
        size = len(lines[0]) - 2, len(lines) - 2
        assert all(len(line) == len(lines[0]) for line in lines)
        assert all(c == "#" for x, c in enumerate(lines[0]) if x != 1)
        assert lines[0][1] == "."
        start = Position(0, -1)
        assert all(c == "#" for x, c in enumerate(lines[-1]) if x != size[0])
        assert lines[size[1] + 1][size[0]] == "."
        end = Position(size[0] - 1, size[1])
        assert all(line[0] == "#" and line[-1] == "#" for line in lines)

        blizzards: set[Blizzard] = set()
        positions: set[Position] = set()
        for y, line in enumerate(lines[1:-1]):
            for x, c in enumerate(line[1:-1]):
                dir = ARROW_DIRECTIONS.get(c)
                if dir is not None:
                    pos = Position(x, y)
                    blizzards.add(Blizzard(pos, dir))
                    positions.add(pos)

        return cls(start, end, size, 0, frozenset(blizzards), frozenset(positions))

    def step(self) -> "Valley":
        new_blizzards = frozenset(b.update_position(self.size) for b in self.blizzards)
        return Valley(
            self.start,
            self.end,
            self.size,
            self.steps + 1,
            new_blizzards,
            frozenset(b.pos for b in new_blizzards),
        )

    def isoccupied(self, pos: Position) -> bool:
        if pos in (self.start, self.end):
            return False
        if pos.x < 0 or pos.x >= self.size[0]:
            return True
        if pos.y < 0 or pos.y >= self.size[1]:
            return True
        return pos in self.occupied

    def print(self, expedition: "Position | None" = None) -> None:
        lines: list[list[str]] = [
            [
                "#"
                if x == 0 or y == 0 or x == self.size[0] + 1 or y == self.size[1] + 1
                else "."
                for x in range(self.size[0] + 2)
            ]
            for y in range(self.size[1] + 2)
        ]
        lines[self.start.y + 1][self.start.x + 1] = "."
        lines[self.end.y + 1][self.end.x + 1] = "."
        for bl in self.blizzards:
            cell = lines[bl.pos.y + 1][bl.pos.x + 1]
            if cell == ".":
                symbol = REVERSE_ARROW_DIRECTIONS[bl.dir]
            elif cell in ARROW_DIRECTIONS:
                symbol = "2"
            elif cell in ("+", "9"):
                symbol = "+"
            else:
                symbol = str(int(cell) + 1)
            lines[bl.pos.y + 1][bl.pos.x + 1] = symbol
        if expedition is not None:
            assert lines[expedition.y + 1][expedition.x + 1] == "."
            lines[expedition.y + 1][expedition.x + 1] = "E"
        print("\n".join("".join(line) for line in lines))


class ValleyHistory:
    _history: list[Valley]

    def __init__(self, valley: Valley) -> None:
        assert valley.steps == 0
        self._history = [valley]

    def __getitem__(self, index: int) -> Valley:
        while len(self._history) <= index:
            self._history.append(self._history[-1].step())
        return self._history[index]


class BlizzardyDijkstraToken:
    pos: Position
    time: int
    distance_to_go: float
    via: "BlizzardyDijkstraToken | None"

    def __init__(
        self,
        pos: Position,
        time: int,
        distance_to_go: float,
        via: "BlizzardyDijkstraToken | None",
    ) -> None:
        self.pos = pos
        self.time = time
        self.distance_to_go = distance_to_go
        self.via = via

    @property
    def sort_key(self) -> float:
        return self.time + self.distance_to_go * 0.9

    def __lt__(self, o: Any) -> bool:
        if isinstance(o, BlizzardyDijkstraToken):
            return self.sort_key < o.sort_key
        return NotImplemented


MOVES = (
    Position(0, 0),
    Position(1, 0),
    Position(0, 1),
    Position(-1, 0),
    Position(0, -1),
)


def blizzardy_dijkstra(valley: Valley) -> tuple[int, Valley]:
    valleys = ValleyHistory(valley)

    queue = [
        BlizzardyDijkstraToken(
            valley.start, 0, (valley.end - valley.start).length, None
        )
    ]
    visited: set[tuple[Position, int]] = set()
    while True:
        token = heapq.heappop(queue)
        if token.pos == valley.end:
            break
        if (token.pos, token.time) in visited:
            continue
        visited.add((token.pos, token.time))

        new_time = token.time + 1
        v = valleys[new_time]
        for dp in MOVES:
            new_pos = token.pos + dp
            if (new_pos, new_time) in visited:
                continue
            if v.isoccupied(new_pos):
                continue
            heapq.heappush(
                queue,
                BlizzardyDijkstraToken(
                    new_pos, new_time, (valley.end - new_pos).length, token
                ),
            )

    # path = [token]
    # pt = token
    # while pt.via is not None:
    #     pt = pt.via
    #     path.insert(0, pt)
    # for t in path:
    #     valleys[t.time].print(t.pos)

    return token.time, valleys[token.time]


def reverse_valley(v: Valley) -> Valley:
    return Valley(
        v.end,
        v.start,
        v.size,
        0,
        v.blizzards,
        v.occupied,
    )


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    VALLEY = Valley.parse_lines(PUZZLE_INPUT)

    p1, last_valley = blizzardy_dijkstra(VALLEY)
    print(f"Part 1: {p1}")

    p2a, last_valley = blizzardy_dijkstra(reverse_valley(last_valley))
    p2b, _ = blizzardy_dijkstra(reverse_valley(last_valley))
    print(f"Part 2: {p1 + p2a + p2b}")
