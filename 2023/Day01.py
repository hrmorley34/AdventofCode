from string import digits

from puzzle_input import puzzle_input

DIGIT_NAMES = {
    "zero": 0,
    "one": 1,
    "two": 2,
    "three": 3,
    "four": 4,
    "five": 5,
    "six": 6,
    "seven": 7,
    "eight": 8,
    "nine": 9,
}


def to_number_p1(line: str) -> int:
    nums = [c for c in line if c in digits]
    return int(nums[0] + nums[-1])


def get_number(string: str, index: int) -> int | None:
    if string[index] in digits:
        return int(string[index])
    for s, i in DIGIT_NAMES.items():
        if s == string[index : index + len(s)]:
            return i
    return None


def to_number_p2(line: str) -> int:
    nums = [
        i
        for i in (get_number(line, index) for index in range(len(line)))
        if i is not None
    ]
    return int(f"{nums[0]}{nums[-1]}")


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    print("Part 1:", sum(map(to_number_p1, PUZZLE_INPUT)))
    print("Part 2:", sum(map(to_number_p2, PUZZLE_INPUT)))
