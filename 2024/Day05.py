from collections import defaultdict
from collections.abc import Mapping

from puzzle_input import puzzle_input


def sort_update(update: list[int], deps: Mapping[int, set[int]]) -> None:
    remaining = set(update)
    subdeps = {i: {di for di in deps[i] if di in update} for i in update}
    new_update: list[int] = []
    while remaining:
        i, *_ = (
            i for i, s in subdeps.items() if i in remaining and not (s & remaining)
        )
        new_update.append(i)
        remaining.remove(i)

    update.clear()
    update.extend(new_update)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    deps, updates = (s.splitlines() for s in PUZZLE_INPUT.split("\n\n"))
    DEPS = defaultdict(set[int])
    for line in deps:
        x, y = map(int, line.split("|"))
        DEPS[y].add(x)
    UPDATES = [list(map(int, line.split(","))) for line in updates]

    total1 = total2 = 0
    for update in UPDATES:
        needs_sort = False
        for index, page in enumerate(update):
            if DEPS[page] & set(update[index + 1 :]):
                needs_sort = True
        if needs_sort:
            sort_update(update, DEPS)
            total2 += update[len(update) // 2]
        else:
            total1 += update[len(update) // 2]
    print("Part 1:", total1)
    print("Part 2:", total2)
