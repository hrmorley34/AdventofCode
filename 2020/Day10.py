from puzzle_input import puzzle_input
from collections import Counter, defaultdict


PUZZLE_INPUT = [int(i) for i in puzzle_input().splitlines()]
sort = tuple(sorted(PUZZLE_INPUT + [max(PUZZLE_INPUT) + 3]))


c = Counter()
sections = []
prevx = 0
lasti = 0
for i, x in enumerate(sort):
    c[x - prevx] += 1
    if x - prevx >= 3:
        sections.append(sort[lasti:i])
        lasti = i
    prevx = x

sections[0] = (0,) + sections[0]

print(c[1] * c[3])


product = 1
for section in sections:
    if len(section) == 1:
        # product *= 1
        continue

    ic = Counter()

    for i, x in enumerate(section):
        if i <= 0:
            ic[x] = 1
            continue
        for x2 in section[:i][-3:]:
            diff = x - x2
            if diff > 3:
                continue
            ic[x] += ic[x2]

    product *= ic[section[-1]]

print(product)
