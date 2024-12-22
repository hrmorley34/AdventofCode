from collections import Counter
from collections.abc import Generator

from puzzle_input import puzzle_input


def mix_prune(a: int, b: int) -> int:
    return (a ^ b) % 16777216


def prng_step(i: int) -> int:
    i = mix_prune(i, i * 64)
    i = mix_prune(i, i // 32)
    i = mix_prune(i, i * 2048)
    return i


def apply_many(i: int, r: int) -> Generator[int, None, None]:
    for _ in range(r):
        i = prng_step(i)
        yield i


def test_subseq(
    subseq: tuple[int, ...], prices: list[int], last_diffs: list[tuple[int, ...]]
) -> int:
    try:
        return prices[last_diffs.index(subseq)]
    except ValueError:
        return 0


if __name__ == "__main__":
    PUZZLE_INPUT = list(map(int, puzzle_input().splitlines()))

    STEPS = [[i] + list(apply_many(i, 2000)) for i in PUZZLE_INPUT]
    print("Part 1:", sum(st[2000] for st in STEPS))

    scores: Counter[tuple[int, ...]] = Counter()
    for steps in STEPS:
        prices = [i % 10 for i in steps]
        diffs = [b - a for a, b in zip(prices, prices[1:])]
        last_diffs = [tuple(diffs[:x][-4:]) for x in range(len(diffs))]
        seen_diffs: set[tuple[int, ...]] = set()
        for price, last_diff in zip(prices, last_diffs):
            if len(last_diff) != 4:
                continue
            if last_diff not in seen_diffs:
                seen_diffs.add(last_diff)
                scores[last_diff] += price
    print("Part 2:", max(scores.values()))
