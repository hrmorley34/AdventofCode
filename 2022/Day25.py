from puzzle_input import puzzle_input

SNAFU_DIGITS = "=-012"


def desnafu_digit(digit: str) -> int:
    return SNAFU_DIGITS.index(digit) - 2


def desnafu(num: str) -> int:
    i = 0
    for digit in num:
        i *= 5
        i += desnafu_digit(digit)
    return i


def ensnafu_digit(digit: int) -> str:
    return SNAFU_DIGITS[digit + 2]


def ensnafu(num: int) -> str:
    i = ""
    while num != 0:
        extra = num % 5
        if extra > 2:
            extra -= 5
        i = ensnafu_digit(extra) + i
        num -= extra
        assert num % 5 == 0
        num //= 5
    if not i:
        i = ensnafu_digit(0)
    return i


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    print(list(map(desnafu, PUZZLE_INPUT)))
    i = ensnafu(sum(map(desnafu, PUZZLE_INPUT)))

    print(f"Part 1: {i}")
