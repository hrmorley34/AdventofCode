from puzzle_input import puzzle_input


PREAMBLE = 25


def check_num(array: list, index: int):
    preamble = array[index - PREAMBLE : index]
    number = array[index]
    for i, a in enumerate(preamble):
        for b in preamble[i:]:
            if a + b == number:
                return True
    return False


PUZZLE_INPUT = [int(x) for x in puzzle_input().splitlines()]

index = PREAMBLE
while check_num(PUZZLE_INPUT, index):
    index += 1

INVALID = PUZZLE_INPUT[index]
print(INVALID)


for a in range(len(PUZZLE_INPUT)):
    for b in range(a, len(PUZZLE_INPUT)):
        r = PUZZLE_INPUT[a : b + 1]
        s = sum(r)
        if s == INVALID:
            print(max(r) + min(r))
        elif s > INVALID + 100:
            # numbers are mostly in ascending order, so we missed it
            continue
