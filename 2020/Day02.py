from puzzle_input import puzzle_input
import re

PUZZLE_INPUT = puzzle_input().splitlines()

password = re.compile(r"^(\d+)-(\d+) ([a-z]): ([a-z]*)$")
count_p1 = 0
count_p2 = 0
for row in PUZZLE_INPUT:
    m = password.match(row)
    if int(m.group(1)) <= m.group(4).count(m.group(3)) <= int(m.group(2)):
        count_p1 += 1
    if (m.group(4)[int(m.group(1)) - 1] == m.group(3)) ^ (
        m.group(4)[int(m.group(2)) - 1] == m.group(3)
    ):
        count_p2 += 1

print("Part 1: {}".format(count_p1))
print("Part 2: {}".format(count_p2))
