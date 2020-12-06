from puzzle_input import puzzle_input
import string

PUZZLE_INPUT = puzzle_input().split("\n\n")

total1, total2 = 0, 0
for rows in PUZZLE_INPUT:
    s1, s2 = set(), set(string.ascii_lowercase)
    for row in rows.splitlines():
        s1 |= set(row)
        s2 &= set(row)
    total1 += len(s1)
    total2 += len(s2)

print(total1)
print(total2)
