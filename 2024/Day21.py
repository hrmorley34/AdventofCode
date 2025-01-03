from collections.abc import Generator, Iterable
from dataclasses import dataclass
from functools import lru_cache
from itertools import permutations, product

from puzzle_input import puzzle_input

DIRECTIONS = {"^": (0, -1), ">": (1, 0), "v": (0, 1), "<": (-1, 0)}


@dataclass(frozen=True)
class Moves:
    dx: int
    dy: int
    first: str | None = None

    @property
    def dpos(self) -> tuple[int, int]:
        return self.dx, self.dy

    def get_directions(self) -> dict[str, int]:
        return {
            k: abs(self.dx) if x else abs(self.dy)
            for k, (x, y) in DIRECTIONS.items()
            if 0 != self.dx == x * abs(self.dx) or 0 != self.dy == y * abs(self.dy)
        }

    def get_sequences(self) -> Generator[str, None, None]:
        d = self.get_directions()
        p = permutations(d)
        if self.first is not None:
            f = self.first
            p = filter(lambda o: o[0] in f, p)
        for order in p:
            yield "".join(c * d[c] for c in order) + "A"

    @staticmethod
    def possible_scripts(moves: Iterable["Moves"]) -> Iterable[str]:
        return map("".join, product(*(m.get_sequences() for m in moves)))


@dataclass(frozen=True)
class Keypad:
    grid: tuple[tuple[str | None, ...], ...]
    start: tuple[int, int]

    @classmethod
    def from_grid(cls, grid: Iterable[Iterable[str | None]]) -> "Keypad":
        g = tuple(map(tuple, grid))
        return cls(
            grid=g,
            start=next(
                iter((line.index("A"), i) for i, line in enumerate(g) if "A" in line)
            ),
        )

    def run_input(self, cmd: str) -> str:
        out = ""
        x, y = self.start
        for c in cmd:
            if c in DIRECTIONS:
                dx, dy = DIRECTIONS[c]
                x += dx
                y += dy
            elif c == "A":
                oc = self.grid[y][x]
                assert oc is not None, (x, y)
                out += oc
            else:
                assert False, c
            assert 0 <= y < len(self.grid), (x, y)
            assert 0 <= x < len(self.grid[y]), (x, y)
            assert self.grid[y][x] is not None
        return out

    @lru_cache(None)
    def find_button(self, button: str) -> tuple[int, int]:
        return next(
            iter(
                (line.index(button), i)
                for i, line in enumerate(self.grid)
                if button in line
            )
        )

    def _moves(self, desired_output: str) -> Generator[Moves, None, None]:
        x, y = self.start
        for oc in desired_output:
            if oc != self.grid[y][x]:
                tx, ty = self.find_button(oc)
                first: str | None = None
                if None in self.grid[y][x:tx] or None in self.grid[y][tx:x]:
                    first = "^v"
                if None in self.grid[ty][x:tx] or None in self.grid[ty][tx:x]:
                    first = "<>"
                # No need to check for other cases in given keypads
                yield Moves(tx - x, ty - y, first=first)
                x, y = tx, ty
            else:
                yield Moves(0, 0)

    @lru_cache(2**5)
    def moves(self, desired_output: str) -> list[Moves]:
        return list(self._moves(desired_output))


KEYPAD_NUMERIC = Keypad.from_grid(
    [
        list("789"),
        list("456"),
        list("123"),
        [None, "0", "A"],
    ]
)
KEYPAD_DIRECTION = Keypad.from_grid(
    [
        [None, "^", "A"],
        ["<", "v", ">"],
    ]
)
KEYPADS1 = [KEYPAD_NUMERIC] + [KEYPAD_DIRECTION] * 2
# no need for third KEYPAD_DIRECTION - we use this directly
KEYPADS2 = [KEYPAD_NUMERIC] + [KEYPAD_DIRECTION] * 25


@lru_cache(2**7)
def _best_multistep(desired_output: str, keypads: tuple[Keypad, ...]) -> int:
    if not keypads:
        return len(desired_output)
    kp = keypads[0]
    moves = kp.moves(desired_output)
    parts: list[int] = []
    for move in moves:
        parts.append(
            min(_best_multistep(seq, keypads[1:]) for seq in move.get_sequences())
        )
    return sum(parts)


def best_multistep(desired_output: str, keypads: Iterable[Keypad]) -> int:
    return _best_multistep(desired_output, tuple(keypads))


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    print(
        "Part 1:",
        sum(int(s[:-1]) * best_multistep(s, KEYPADS1) for s in PUZZLE_INPUT),
    )
    print(
        "Part 2:",
        sum(int(s[:-1]) * best_multistep(s, KEYPADS2) for s in PUZZLE_INPUT),
    )
