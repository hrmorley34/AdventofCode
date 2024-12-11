from collections import Counter
from functools import lru_cache
from itertools import chain


def blink1(stone: int) -> list[int]:
    if stone == 0:
        return [1]
    elif len(str(stone)) % 2 == 0:
        return [
            int(str(stone)[: len(str(stone)) // 2]),
            int(str(stone)[len(str(stone)) // 2 :]),
        ]
    else:
        return [stone * 2024]


def blink(stones: list[int]) -> list[int]:
    result: list[int] = []
    for stone in stones:
        result.extend(blink1(stone))
    return result


@lru_cache
def blink5(stone: int) -> list[int]:
    stones = [stone]
    for _ in range(5):
        stones = blink(stones)
    return stones


def blink_counter(stones: Counter[int], n: int = 1) -> Counter[int]:
    new_stones: Counter[int] = Counter()
    for stone, count in stones.items():
        substones = [stone]
        for _ in range(n // 5):
            substones = list(chain.from_iterable(map(blink5, substones)))
        for _ in range(n % 5):
            substones = blink(substones)
        c = Counter(substones)
        for k in c:
            new_stones[k] += c[k] * count
    return new_stones


if __name__ == "__main__":
    PUZZLE_INPUT = input("> ")
    stones = Counter(map(int, PUZZLE_INPUT.split()))

    for _ in range(25 // 5):
        stones = blink_counter(stones, 5)
    print("Part 1:", sum(stones.values()))

    for _ in range((75 - 25) // 5):
        stones = blink_counter(stones, 5)
    print("Part 2:", sum(stones.values()))
