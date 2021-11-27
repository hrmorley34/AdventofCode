from math import log10
from puzzle_input import puzzle_input
import re
from typing import Sequence


CMDRE = re.compile(
    r"^(?:(?P<cut>cut (?P<cut_n>-?\d+))|(?P<dwi>deal with increment (?P<dwi_n>\d+))|(?P<din>deal into new stack))$"
)


Arrangement = dict[int, int]


def factory_order(size: int) -> Arrangement:
    return {i: i for i in range(size)}


def print_arrangement(arr: Arrangement):
    print(" ".join(str(arr[i]) for i in range(len(arr))))


def _apply_arrangement(original: Arrangement, change: Arrangement) -> Arrangement:
    return {k: change[original[k]] for k in range(len(original))}


def apply_arrangement(
    original: Arrangement, change: Arrangement, times: int = 1
) -> Arrangement:
    assert len(original) == len(change)
    if times < 0:
        raise Exception
    else:
        for _ in range(times):
            original = _apply_arrangement(original, change)
        return original


def deal_into_new(start: Arrangement) -> Arrangement:
    count = len(start)
    return {count - k - 1: start[k] for k in range(count)}


def cut(n: int, start: Arrangement) -> Arrangement:
    count = len(start)
    return {(k - n) % count: start[k] for k in range(count)}


def deal_with_increment(n: int, start: Arrangement) -> Arrangement:
    count = len(start)
    return {(k * n) % count: start[k] for k in range(count)}


def parse_command_match(m: re.Match[str], start: Arrangement) -> Arrangement:
    groups = m.groupdict()
    if groups.get("cut"):
        return cut(int(groups["cut_n"]), start)
    elif groups.get("dwi"):
        return deal_with_increment(int(groups["dwi_n"]), start)
    elif groups.get("din"):
        return deal_into_new(start)
    else:
        raise Exception("No command")


def parse_commands(cmds: str | Sequence[str], start: Arrangement) -> Arrangement:
    if isinstance(cmds, str):
        cmds = cmds.splitlines()

    for cmd in cmds:
        m = CMDRE.match(cmd)
        if m is None:
            raise Exception("No match")
        start = parse_command_match(m, start=start)
    return start


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()

    COUNT = 10007
    deck = factory_order(COUNT)
    order = parse_commands(PUZZLE_INPUT, deck)
    for k, v in order.items():
        if v == 2019:
            print("Part 1:", k)

    COUNT2 = 119315717514047
    ITERATIONS2 = 101741582076661
    deck = factory_order(COUNT2)
    print("Generated factory order")
    POWS: dict[int, Arrangement] = {0: parse_commands(PUZZLE_INPUT, deck)}
    print("Generated power 0")
    for p in range(1, int(log10(ITERATIONS2)) + 1):
        POWS[p] = apply_arrangement(POWS[p - 1], POWS[p - 1], 9)
        print(f"Generated power {p}")
    # POWS[0] -> 10^0 = 1 iteration
    # POWS[1] -> 10^1 = 10 iterations
    # POWS[2] -> 10^2 = 100 iterations
    print("Generated powers")

    order2 = deck.copy()
    for power, digit in enumerate(map(int, reversed(str(ITERATIONS2)))):
        order2 = apply_arrangement(order2, POWS[power], digit)

    print("Part 2:", order2[2020])
