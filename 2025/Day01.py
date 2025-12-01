from puzzle_input import puzzle_input


def parse_dial(s: str) -> int:
    mul = 1 if s[0] == "R" else -1
    return mul * int(s[1:])


def accumulate_zeroes(changes: list[int]) -> tuple[int, int]:
    value = 50
    zeroes = 0
    zeropass = 0
    for ch in changes:
        last_value = value
        zeropass += abs(ch) // 100
        value_nomod = last_value + (abs(ch) % 100) * (ch // abs(ch))
        value = value_nomod % 100
        if value == 0:
            zeroes += 1
            zeropass += 1
        elif value != value_nomod and last_value != 0:
            zeropass += 1
    return zeroes, zeropass


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    values = list(map(parse_dial, PUZZLE_INPUT))
    p1, p2 = accumulate_zeroes(values)
    print("Part 1:", p1)
    print("Part 2:", p2)
