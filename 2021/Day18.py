from __future__ import annotations

from dataclasses import dataclass, replace
from puzzle_input import puzzle_input
from typing import Any, cast


SPLIT_THRESHOLD = 10


@dataclass(init=True, eq=True, frozen=True)
class Pair:
    x: int | Pair
    y: int | Pair

    def __repr__(self) -> str:
        return f"[{self.x!r},{self.y!r}]"

    def has_pairs(self) -> bool:
        return isinstance(self.x, Pair) or isinstance(self.y, Pair)

    @staticmethod
    def spliti(i: int) -> Pair:
        return Pair(i // 2, (i + 1) // 2)

    def split(self) -> Pair | None:
        for key, value in (("x", self.x), ("y", self.y)):
            if isinstance(value, Pair):
                s = value.split()
                if s is not None:
                    return replace(self, **{key: s})
            else:
                if value >= SPLIT_THRESHOLD:
                    return replace(self, **{key: self.spliti(value)})
        return None

    def _add(
        self, value: int, right: bool, immediate_right: bool | None = None
    ) -> Pair:
        if immediate_right is None:
            immediate_right = right

        if immediate_right:
            if isinstance(self.y, Pair):
                return replace(self, y=self.y._add(value, right=right))
            else:
                return replace(self, y=self.y + value)
        else:
            if isinstance(self.x, Pair):
                return replace(self, x=self.x._add(value, right=right))
            else:
                return replace(self, x=self.x + value)

    def explode(self) -> Pair | None:
        v = self._explode(0)
        if v is None:
            return None
        assert isinstance(v[0], Pair)
        return v[0]

    def _explode(
        self, nest: int
    ) -> tuple[Pair | int, tuple[int | None, int | None]] | None:
        if not self.has_pairs():
            if nest >= 4:
                return 0, (cast(int, self.x), cast(int, self.y))
            else:
                return None
        for key, value in (("x", self.x), ("y", self.y)):
            if isinstance(value, Pair):
                v = value._explode(nest + 1)
                if v is not None:
                    pair, (left, right) = v
                    self = replace(self, **{key: pair})
                    if key == "y" and left is not None:
                        self = self._add(left, right=True, immediate_right=False)
                        left = None
                    elif key == "x" and right is not None:
                        self = self._add(right, right=False, immediate_right=True)
                        right = None
                    return (self, (left, right))
        return None

    def reduce(self) -> Pair:
        pair = self
        while True:
            expl = pair.explode()
            if expl is not None:
                pair = expl
                continue
            spl = pair.split()
            if spl is not None:
                pair = spl
                continue
            break
        return pair

    def __add__(self, obj: Any) -> Pair:
        if isinstance(obj, Pair):
            return Pair(self, obj)
        return NotImplemented

    @classmethod
    def parse_string(cls, s: str) -> Pair:
        valuestack: list[Pair | int | None] = [None]
        for c in s:
            # print(c, valuestack)
            if c == "[":
                valuestack.append(None)
            elif c == "]":
                x, y = valuestack.pop(-2), valuestack.pop(-1)
                assert x is not None
                assert y is not None
                assert valuestack[-1] is None
                valuestack[-1] = Pair(x, y)
            elif c == ",":
                valuestack.append(None)
            else:
                last = valuestack[-1]
                if last is None:
                    last = 0
                assert not isinstance(last, Pair)
                valuestack[-1] = last * 10 + int(c)
        # print(valuestack)
        assert len(valuestack) == 1
        item = valuestack[0]
        assert isinstance(item, Pair)
        return item

    def get_magnitude(self) -> int:
        total = 0
        if isinstance(self.x, Pair):
            total += 3 * self.x.get_magnitude()
        else:
            total += 3 * self.x
        if isinstance(self.y, Pair):
            total += 2 * self.y.get_magnitude()
        else:
            total += 2 * self.y
        return total


if __name__ == "__main__":
    PUZZLE_INPUT = list(map(Pair.parse_string, puzzle_input().splitlines()))

    total = PUZZLE_INPUT[0]
    for pair in PUZZLE_INPUT[1:]:
        # print(total)
        total = (total + pair).reduce()

    # print(total)
    print("Part 1:", total.get_magnitude())

    best: int = 0
    s = set(PUZZLE_INPUT)
    for a in s:
        for b in s - {a}:
            total = (a + b).reduce().get_magnitude()
            if total > best:
                best = total
    print("Part 2:", best)
