import operator as op
from functools import reduce

from puzzle_input import puzzle_input

OPERATORS = {"+": op.add, "*": op.mul}
if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    ops = PUZZLE_INPUT[-1].split()

    values_rows = [list(map(int, line.split())) for line in PUZZLE_INPUT[:-1]]
    cols = zip(ops, zip(*values_rows))
    results = [reduce(OPERATORS[opn], vs) for opn, vs in cols]
    print("Part 1:", sum(results))

    PI_T = list(zip(*PUZZLE_INPUT[:-1]))
    values_cols: list[list[int]] = [[]]
    for col in PI_T:
        if all(c == " " for c in col):
            values_cols.append([])
        else:
            values_cols[-1].append(int("".join(col)))
    cols2 = list(zip(ops, values_cols))
    results2 = [reduce(OPERATORS[opn], vs) for opn, vs in cols2]
    print("Part 2:", sum(results2))
