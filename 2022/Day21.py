import operator
import re
from typing import Callable, Iterable
from weakref import WeakKeyDictionary
from fractions import Fraction

from puzzle_input import puzzle_input

RE_LINE = re.compile(r"^(\w{4}): (?:(-?\d+)|(\w{4}) ([\+\-\*/]) (\w{4}))$")


def safediv(a: "NUMTYPE", b: "NUMTYPE") -> "NUMTYPE":
    assert not isinstance(b, Variable)
    if isinstance(a, int):
        return Fraction(a, b)
    return a / b


OPERATORS = {
    "+": operator.add,
    "-": operator.sub,
    "*": operator.mul,
    "/": safediv,
}


class ElementMap:
    _map: dict[str, "Element"]

    def __init__(self, el: Iterable["Element"]) -> None:
        self._map = {e.name: e for e in el}

    def get(self, v: str) -> "NUMTYPE":
        return self._map[v].func(self)

    def __getitem__(self, v: str) -> "Element":
        return self._map[v]

    def copy(self):
        return type(self)(el.copy() for el in self._map.values())


class Element:
    name: str
    _op: Callable[["NUMTYPE", "NUMTYPE"], "NUMTYPE"]
    _func: Callable[["Element", ElementMap], "NUMTYPE"]
    _cache: WeakKeyDictionary[ElementMap, "NUMTYPE"]

    def __init__(self) -> None:
        self._cache = WeakKeyDictionary()

    def copy(self):
        nself = type(self)()
        nself.name = self.name
        nself._func = self._func
        nself._op = self._op
        return nself

    @classmethod
    def from_line(cls, s: str):
        self = cls()

        m = RE_LINE.match(s)
        assert m
        self.name = m[1]
        if m[2]:
            value = int(m[2])
            self._func = lambda _s, _e: value
            self._op = lambda _a, _b: 0  # dummy
        else:
            a, self._op, b = m[3], OPERATORS[m[4]], m[5]

            def f(s: Element, e: ElementMap) -> NUMTYPE:
                return s._op(e.get(a), e.get(b))

            self._func = f

        return self

    def func(self, e: ElementMap) -> "NUMTYPE":
        if e in self._cache:
            return self._cache[e]
        v = self._func(self, e)
        self._cache[e] = v
        return v


class Variable:
    name: str
    coefficients: dict[int, Fraction]

    def __init__(
        self, name: str, coefficients: dict[int, Fraction] | None = None
    ) -> None:
        self.name = name
        self.coefficients = {1: Fraction(1)} if coefficients is None else coefficients

    def copy(self):
        return type(self)(self.name, self.coefficients.copy())

    def __add__(self, b: "NUMTYPE"):
        if isinstance(b, Variable):
            assert self.name == b.name
            v = self.copy()
            for k, i in b.coefficients.items():
                v.coefficients[k] += i
            return v
        else:
            v = self.copy()
            v.coefficients[0] = v.coefficients.get(0, Fraction(0)) + b
            return v

    def __radd__(self, b: int | Fraction):
        return self + b

    def __sub__(self, b: "NUMTYPE"):
        return self + (-b)

    def __rsub__(self, b: "NUMTYPE"):
        return b + (-self)

    def __neg__(self):
        return type(self)(self.name, {k: -i for k, i in self.coefficients.items()})

    def __mul__(self, b: "NUMTYPE"):
        if isinstance(b, Variable):
            assert self.name == b.name
            v = type(self)(self.name, {})
            for ka, ia in self.coefficients.items():
                for kb, ib in b.coefficients.items():
                    v.coefficients[ka + kb] += ia * ib
            return v
        else:
            return type(self)(
                self.name, {k: i * b for k, i in self.coefficients.items()}
            )

    def __rmul__(self, b: int | Fraction):
        return self * b

    def __truediv__(self, b: int | Fraction):
        return type(self)(self.name, {k: i / b for k, i in self.coefficients.items()})

    def __repr__(self) -> str:
        parts: list[str] = []
        for k, v in sorted(self.coefficients.items(), reverse=True):
            if v == 0:
                continue
            if k == 0:
                parts.append(f"({v})")
            elif k == 1:
                parts.append(f"{v}*{self.name}")
            else:
                parts.append(f"{v}*{self.name}**{k}")
        return " + ".join(parts)


NUMTYPE = int | Variable | Fraction


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    ELEMENTMAP = ElementMap(map(Element.from_line, PUZZLE_INPUT))
    root = ELEMENTMAP.get("root")
    print(f"Part 1: {root}")

    EL2 = ELEMENTMAP.copy()
    EL2["humn"]._func = lambda _s, _e: Variable("x")
    EL2["root"]._op = operator.sub  # and then set equal to zero
    root = EL2.get("root")

    if isinstance(root, Variable):
        nonzero_keys = {k for k, v in root.coefficients.items() if v}
        assert nonzero_keys == {0, 1}
        # ax+b = 0 => ax = -b => x = -b/a
        root = -root.coefficients[0] / root.coefficients[1]
        assert not isinstance(root, Variable)

    print(f"Part 2: {root}")
