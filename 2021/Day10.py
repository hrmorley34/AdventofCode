from dataclasses import dataclass
from puzzle_input import puzzle_input
from typing import Iterable, NewType, Sequence, cast


OpeningBracket = NewType("OpeningBracket", str)
ClosingBracket = NewType("ClosingBracket", str)
Bracket = OpeningBracket | ClosingBracket


class SuccessMode:
    success: bool


class Success(SuccessMode):
    success = True


class Failure(SuccessMode):
    success = False


@dataclass
class Incomplete(Failure):
    missing: list[ClosingBracket]


@dataclass
class Corrupted(Failure):
    firstbroken: ClosingBracket
    broken: list[Bracket]
    missing: list[ClosingBracket]


DELIMITERS = cast(
    set[tuple[OpeningBracket, ClosingBracket]],
    {("(", ")"), ("[", "]"), ("{", "}"), ("<", ">")},
)
OPENERS = {t[0] for t in DELIMITERS}
CLOSERS = {t[1] for t in DELIMITERS}
OPENING = {t[0]: t[1] for t in DELIMITERS}
CLOSING = {t[1]: t[0] for t in DELIMITERS}


SYNTAX_POINTS = cast(
    dict[ClosingBracket, int], {")": 3, "]": 57, "}": 1197, ">": 25137}
)
AUTOCOMP_POINTS = cast(dict[ClosingBracket, int], {")": 1, "]": 2, "}": 3, ">": 4})


def opened_to_closed(it: Iterable[OpeningBracket]) -> list[ClosingBracket]:
    return list(reversed([OPENING[b] for b in it]))


def parse(brackets: Iterable[Bracket]) -> SuccessMode:
    it = iter(brackets)
    openbrackets: list[OpeningBracket] = list()
    for b in it:
        if b in OPENERS:
            openbrackets.append(b)
        elif b in CLOSERS:
            if openbrackets[-1] == CLOSING[b]:
                openbrackets.pop(-1)
            else:
                return Corrupted(
                    firstbroken=b,
                    broken=[b, *it],
                    missing=opened_to_closed(openbrackets),
                )
        else:
            raise ValueError(f"Unknown bracket {b}")
    if openbrackets:
        return Incomplete(missing=opened_to_closed(openbrackets))
    else:
        return Success()


def calc_autocomp_points(brackets: Iterable[ClosingBracket]) -> int:
    score = 0
    for bracket in brackets:
        score *= 5
        score += AUTOCOMP_POINTS[bracket]
    return score


if __name__ == "__main__":
    PUZZLE_INPUT = cast(list[Sequence[Bracket]], puzzle_input().splitlines())

    part1 = 0
    part2: list[int] = []
    for line in map(parse, PUZZLE_INPUT):
        if isinstance(line, Corrupted):
            part1 += SYNTAX_POINTS[line.firstbroken]
        elif isinstance(line, Incomplete):
            part2.append(calc_autocomp_points(line.missing))
    print("Part 1:", part1)
    print("Part 2:", sorted(part2)[(len(part2) - 1) // 2])
