from puzzle_input import puzzle_input


def find_rating(array: list[str], least_common: bool = False, index: int = 0) -> str:
    filter0 = [row for row in array if row[index] == "0"]
    filter1 = [row for row in array if row[index] == "1"]
    if len(filter1) >= len(filter0):
        out = filter0 if least_common else filter1
    else:
        out = filter1 if least_common else filter0
    if len(out) == 1:
        return out[0]
    else:
        return find_rating(out, least_common=least_common, index=index + 1)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    gamma = epsilon = ""
    for digit in zip(*PUZZLE_INPUT):
        if digit.count("1") > len(digit) / 2:
            gamma += "1"
            epsilon += "0"
        else:
            gamma += "0"
            epsilon += "1"
    print("Part 1:", int(gamma, 2) * int(epsilon, 2))

    print(
        "Part 2:",
        int(find_rating(PUZZLE_INPUT, False), 2)
        * int(find_rating(PUZZLE_INPUT, True), 2),
    )
