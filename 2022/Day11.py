import re
from math import lcm
from typing import Callable

from puzzle_input import puzzle_input

MONKEY_RE = re.compile(
    r"^Monkey (?P<index>\d+):\n"
    r"  Starting items: (?P<starting_items>\d+(?:, \d+)*)\n"
    r"  Operation: new = (?P<operation>.*?)\n"
    r"  Test: divisible by (?P<test_div>\d+)\n"
    r"    If true: throw to monkey (?P<test_dest_true>\d+)\n"
    r"    If false: throw to monkey (?P<test_dest_false>\d+)$"
)


class Monkey:
    index: int
    starting_items: list[int]
    operation: Callable[[int], int]
    test_div: int
    test_dest: tuple[int, int]  # dest: true, false

    inspections: int

    def __init__(self) -> None:
        self.inspections = 0

    @classmethod
    def from_string(cls, s: str):
        m = MONKEY_RE.match(s)
        assert m
        self = cls()
        self.index = int(m["index"])
        self.starting_items = [int(x) for x in m["starting_items"].split(", ")]
        self.operation = eval("lambda old: " + m["operation"])  # VERY UNSAFE
        self.test_div = int(m["test_div"])
        self.test_dest = (int(m["test_dest_true"]), int(m["test_dest_false"]))
        return self

    def throw_item_1(self, item: int) -> tuple[int, int]:
        self.inspections += 1
        new = self.operation(item) // 3
        if new % self.test_div == 0:
            return (new, self.test_dest[0])
        else:
            return (new, self.test_dest[1])

    def throw_item_2(self, item: int, base: int) -> tuple[int, int]:
        self.inspections += 1
        new = self.operation(item) % base
        if new % self.test_div == 0:
            return (new, self.test_dest[0])
        else:
            return (new, self.test_dest[1])


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().split("\n\n")

    monkeys = [Monkey.from_string(s) for s in PUZZLE_INPUT]
    items1 = [m.starting_items.copy() for m in monkeys]

    for _ in range(20):
        for m, its in zip(monkeys, items1):
            while its:
                new_it, dest = m.throw_item_1(its.pop(0))
                items1[dest].append(new_it)

    a, b = sorted(m.inspections for m in monkeys)[-2:]
    monkey_business = a * b

    print(f"Part 1: {monkey_business}")

    # reset inspections
    monkeys = [Monkey.from_string(s) for s in PUZZLE_INPUT]
    items2 = [m.starting_items.copy() for m in monkeys]
    mod2 = lcm(*(m.test_div for m in monkeys))

    for _ in range(10_000):
        for m, its in zip(monkeys, items2):
            while its:
                new_it, dest = m.throw_item_2(its.pop(0), mod2)
                items2[dest].append(new_it)

    a, b = sorted(m.inspections for m in monkeys)[-2:]
    monkey_business = a * b

    print(f"Part 2: {monkey_business}")
