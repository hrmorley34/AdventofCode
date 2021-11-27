from __future__ import annotations

from math import log10
from puzzle_input import puzzle_input
import re
from typing import Any, Sequence, overload


CMDRE = re.compile(
    r"^(?:(?P<cut>cut (?P<cut_n>-?\d+))|(?P<dwi>deal with increment (?P<dwi_n>\d+))|(?P<din>deal into new stack))$"
)


class Func:
    size: int

    def __init__(self, size: int) -> None:
        self.size = size

    def apply(self, position: int) -> int:
        return self.apply_nomod(position) % self.size

    def apply_nomod(self, position: int) -> int:
        raise NotImplementedError

    def rapply(self, position: int) -> int:
        return self.rapply_nomod(position) % self.size

    def rapply_nomod(self, position: int) -> int:
        raise NotImplementedError


class DealIntoNew(Func):
    def apply_nomod(self, position: int) -> int:
        return -position - 1

    def rapply_nomod(self, position: int) -> int:
        return -position - 1


class _FuncN(Func):
    n: int

    def __init__(self, size: int, n: int) -> None:
        super().__init__(size)
        self.n = n


class Cut(_FuncN):
    def apply_nomod(self, position: int) -> int:
        return position - self.n

    def rapply_nomod(self, position: int) -> int:
        return position + self.n


class DealWithIncrement(_FuncN):
    def apply_nomod(self, position: int) -> int:
        return position * self.n

    def rapply_nomod(self, position: int) -> int:
        return position * pow(self.n, -1, self.size)


class FuncStack(Func, list[Func]):
    def __init__(self, size: int) -> None:
        super().__init__(size)

    def apply(self, position: int) -> int:
        for f in self:
            position = f.apply(position)
        return position

    def apply_nomod(self, position: int) -> int:
        for f in self:
            position = f.apply_nomod(position)
        return position

    def rapply(self, position: int) -> int:
        for f in reversed(self):
            position = f.rapply(position)
        return position

    def rapply_nomod(self, position: int) -> int:
        for f in reversed(self):
            position = f.rapply_nomod(position)
        return position


def parse_command_match(m: re.Match[str], size: int) -> Func:
    groups = m.groupdict()
    if groups.get("cut"):
        return Cut(size, int(groups["cut_n"]))
    elif groups.get("dwi"):
        return DealWithIncrement(size, int(groups["dwi_n"]))
    elif groups.get("din"):
        return DealIntoNew(size)
    else:
        raise Exception("No command")


def parse_commands(cmds: str | Sequence[str], size: int) -> FuncStack:
    if isinstance(cmds, str):
        cmds = cmds.splitlines()

    funcs = FuncStack(size)
    for cmd in cmds:
        m = CMDRE.match(cmd)
        if m is None:
            raise Exception("No match")
        funcs.append(parse_command_match(m, size=size))
    return funcs


class Poly1:
    m: int
    c: int

    def __new__(cls, m: int = 1, c: int = 0):
        if m == 0:
            return c
        return super().__new__(cls)

    def __init__(self, m: int = 1, c: int = 0):
        self.m = m
        self.c = c

    def __repr__(self) -> str:
        s = f"Poly1({self.m}*x"
        if self.c:
            s += f" + {self.c}"
        return s + ")"

    def __neg__(self) -> Poly1:
        return Poly1(-self.m, -self.c)

    def __abs__(self) -> Poly1:
        return Poly1(abs(self.m), abs(self.c))

    @overload
    def __add__(self, obj: Poly1) -> Poly1:
        ...

    @overload
    def __add__(self, obj: int) -> Poly1:
        ...

    def __add__(self, obj: Any) -> Any:
        if isinstance(obj, Poly1):
            return Poly1(self.m + obj.m, self.c + obj.c)
        elif isinstance(obj, int):
            return Poly1(self.m, self.c + obj)
        return NotImplemented

    @overload
    def __radd__(self, obj: Poly1) -> Poly1:
        ...

    @overload
    def __radd__(self, obj: int) -> Poly1:
        ...

    def __radd__(self, obj: Any) -> Any:
        return self + obj

    @overload
    def __sub__(self, obj: Poly1) -> Poly1:
        ...

    @overload
    def __sub__(self, obj: int) -> Poly1:
        ...

    def __sub__(self, obj: Any) -> Any:
        return self + (-obj)

    @overload
    def __rsub__(self, obj: Poly1) -> Poly1:
        ...

    @overload
    def __rsub__(self, obj: int) -> Poly1:
        ...

    def __rsub__(self, obj: Any) -> Any:
        return (-self) + obj

    def __mul__(self, obj: Any) -> Any:
        if isinstance(obj, Poly1):
            raise NotImplementedError
        elif isinstance(obj, int):
            return Poly1(self.m * obj, self.c * obj)
        return NotImplemented

    def __rmul__(self, obj: Any) -> Any:
        return self * obj

    def __mod__(self, obj: Any) -> Any:
        if isinstance(obj, int):
            return Poly1(self.m % obj, self.c % obj)
        return NotImplemented

    @overload
    def apply(self, value: Poly1) -> Poly1:
        ...

    @overload
    def apply(self, value: int) -> int:
        ...

    def apply(self, value: Any) -> Any:
        return self.m * value + self.c


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()

    COUNT = 10007
    funcs = parse_commands(PUZZLE_INPUT, COUNT)
    print("Part 1:", funcs.apply(2019))

    COUNT2 = 119315717514047
    funcs2 = parse_commands(PUZZLE_INPUT, COUNT2)
    ITERATIONS2 = 101741582076661

    POWS: dict[int, Poly1] = {0: funcs2.rapply_nomod(Poly1(1)) % COUNT2}
    # print("Generated power 0")
    for p in range(1, int(log10(ITERATIONS2)) + 1):
        np = Poly1(1)
        for _ in range(10):
            np = POWS[p - 1].apply(np)
        POWS[p] = np % COUNT2
        # print(f"Generated power {p}")
    # POWS[0] -> 10^0 = 1 iteration
    # POWS[1] -> 10^1 = 10 iterations
    # POWS[2] -> 10^2 = 100 iterations
    # print("Generated powers")

    value = 2020
    for power, digit in enumerate(map(int, reversed(str(ITERATIONS2)))):
        for _ in range(digit):
            value = POWS[power].apply(value) % COUNT2

    print("Part 2:", value)
