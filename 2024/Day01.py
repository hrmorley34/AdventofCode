from collections import Counter

from puzzle_input import puzzle_input

if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    LIST1, LIST2 = map(list[int], zip(*(map(int, s.split()) for s in PUZZLE_INPUT)))
    LIST1.sort()
    LIST2.sort()

    assert len(LIST1) == len(LIST2)

    print("Part 1:", sum(abs(i1 - i2) for i1, i2 in zip(LIST1, LIST2)))

    right_counts = Counter(LIST2)
    print("Part 2:", sum(i * right_counts[i] for i in LIST1))
