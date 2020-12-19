from puzzle_input import puzzle_input
from collections import defaultdict
import re


RE_RULELINE = re.compile(r"^(\d+):\s*(((\d+|\"\w+\")\s*)+(\|\s*((\d+|\"\w+\")\s*)+)*)$")
RE_RULE = re.compile(r"(\d+)|\"(\w+)\"")


def rules(rules: list[str]) -> dict:
    d = defaultdict(list)
    for rule in rules:
        m = RE_RULELINE.match(rule)
        num = int(m[1])
        for part in m[2].split("|"):
            partl = []
            for m2 in RE_RULE.finditer(part):
                if m2[1]:
                    partl.append(int(m2[1]))
                else:
                    partl.append(m2[2])
            d[num].append(tuple(partl))
    return d


MAX_RECURSIVE_DEPTH = 100


def reify(ruled: dict, rule: int = 0, depth: int = 0) -> str:
    if depth > MAX_RECURSIVE_DEPTH:
        return "(?:\x00)"
    exprs = []
    for part in ruled[rule]:
        exprs.append("")
        for p in part:
            if isinstance(p, str):
                exprs[-1] += re.escape(p)
            else:
                exprs[-1] += reify(ruled, p, depth + 1)
    return "(?:" + "|".join(exprs) + ")"


MODRULES2 = """\
8: 42 | 42 8
11: 42 31 | 42 11 31""".splitlines()

if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    RULES, MESSAGES = PUZZLE_INPUT.split("\n\n")

    RULEDICT = rules(RULES.splitlines())
    expr = re.compile("^" + reify(RULEDICT) + "$")

    RULEDICT2 = {**RULEDICT, **rules(MODRULES2)}
    expr2 = re.compile("^" + reify(RULEDICT2) + "$")

    count, count2 = 0, 0
    for msg in MESSAGES.splitlines():
        m = expr.match(msg)
        if m:
            count += 1
        m2 = expr2.match(msg)
        if m2:
            count2 += 1

    print(count)
    print(count2)
