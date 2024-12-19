import re
from collections.abc import Collection, Generator
from functools import cache

from puzzle_input import puzzle_input


def get_patterns(
    towels: Collection[str], sequence: str
) -> Generator[tuple[str, ...], None, None]:
    if not sequence:
        yield ()
        return
    for towel in towels:
        if towel == sequence[: len(towel)]:
            for t in get_patterns(towels, sequence[len(towel) :]):
                yield (towel, *t)


def has_pattern(towels: Collection[str], sequence: str) -> bool:
    try:
        next(get_patterns(towels, sequence))
    except StopIteration:
        return False
    else:
        return True


@cache
def count_patterns(towels: frozenset[str], sequence: str) -> int:
    if not sequence:
        return 1
    result = 0
    for towel in towels:
        if towel == sequence[: len(towel)]:
            result += count_patterns(towels, sequence[len(towel) :])
    return result


if __name__ == "__main__":
    t, e, *PUZZLE_INPUT = puzzle_input().splitlines()
    assert not e
    TOWELS = frozenset(t.split(", "))

    RE_TOWEL = re.compile(r"^(" + r"|".join(map(re.escape, TOWELS)) + r")*$")
    print("Part 1:", sum(has_pattern(TOWELS, line) for line in PUZZLE_INPUT))
    print("Part 2:", sum(count_patterns(TOWELS, line) for line in PUZZLE_INPUT))
