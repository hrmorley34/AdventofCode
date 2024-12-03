import re

from puzzle_input import puzzle_input

RE_INSTRUCTION1 = re.compile(r"mul\((\d+),(\d+)\)")
RE_INSTRUCTION2 = re.compile(r"mul\((\d+),(\d+)\)|(do(?:n't)?)\(\)")


def parse_instruction1(match: re.Match[str]) -> int:
    return int(match[1]) * int(match[2])


def parse_instruction2(match: re.Match[str], mode: bool) -> tuple[int, bool]:
    if match[3] is not None:
        return 0, match[3] == "do"
    elif mode:
        return int(match[1]) * int(match[2]), True
    else:
        return 0, False


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()

    print(
        "Part 1:", sum(map(parse_instruction1, RE_INSTRUCTION1.finditer(PUZZLE_INPUT)))
    )

    enabled = True
    total = 0
    for instruction in RE_INSTRUCTION2.finditer(PUZZLE_INPUT):
        i, enabled = parse_instruction2(instruction, enabled)
        total += i
    print("Part 2:", total)
