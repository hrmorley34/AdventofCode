import re
from dataclasses import dataclass
from math import prod

from puzzle_input import puzzle_input


@dataclass
class Rule:
    category: str
    op: str
    value: int
    dest: str

    @classmethod
    def from_rule(cls, rule: str):
        m = re.match(r"([xmas])([<>])(\d+):(\w+)", rule)
        assert m is not None
        return cls(category=m[1], op=m[2], value=int(m[3]), dest=m[4])


RuleDict = dict[str, "Rules"]


@dataclass
class Rules:
    name: str
    parts: list[Rule]
    dest: str

    @classmethod
    def from_line(cls, line: str):
        m = re.match(r"(\w+)\{((?:[xmas][<>]\d+:\w+,)*)(\w+)\}", line)
        assert m is not None
        return cls(
            name=m[1], parts=list(map(Rule.from_rule, m[2][:-1].split(","))), dest=m[3]
        )

    @classmethod
    def from_lines(cls, lines: list[str]) -> RuleDict:
        return {r.name: r for r in map(cls.from_line, lines)}


@dataclass
class Data:
    x: int
    m: int
    a: int
    s: int

    @classmethod
    def from_line(cls, line: str):
        m = re.match(r"\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}", line)
        assert m is not None
        return cls(int(m[1]), int(m[2]), int(m[3]), int(m[4]))

    @classmethod
    def from_lines(cls, lines: list[str]) -> list["Data"]:
        return list(map(cls.from_line, lines))

    def check(self, rd: RuleDict) -> bool:
        pos = "in"
        while pos not in ("R", "A"):
            rules = rd[pos]
            for rule in rules.parts:
                v = getattr(self, rule.category)
                comp = (v > rule.value) if rule.op == ">" else (v < rule.value)
                if comp:
                    pos = rule.dest
                    break
            else:
                pos = rules.dest
        return pos == "A"


def search_tree(rd: RuleDict, ranges: dict[str, tuple[int, int]], pos: str) -> int:
    if pos == "R":
        return 0
    elif pos == "A":
        return prod(b - a + 1 for a, b in ranges.values())
    total = 0
    rules = rd[pos]
    for rule in rules.parts:
        v = ranges[rule.category]
        if v[1] < rule.value or v[0] > rule.value:
            continue
        is_gt = rule.op == ">"
        pairs = (v[0], rule.value - 1 + is_gt), (rule.value + is_gt, v[1])
        subranges = ranges.copy()
        subranges[rule.category] = pairs[is_gt]
        ranges[rule.category] = pairs[not is_gt]

        total += search_tree(rd, subranges, rule.dest)

    total += search_tree(rd, ranges, rules.dest)
    return total


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().split("\n\n")
    _rules, _data = PUZZLE_INPUT
    rules = Rules.from_lines(_rules.splitlines())
    data = Data.from_lines(_data.splitlines())

    print("Part 1:", sum(d.x + d.m + d.a + d.s for d in data if d.check(rules)))

    pos = "in"
    init_range = (1, 4000)
    ranges: dict[str, tuple[int, int]] = {
        "x": init_range,
        "m": init_range,
        "a": init_range,
        "s": init_range,
    }
    print("Part 2:", search_tree(rules, ranges, pos))
