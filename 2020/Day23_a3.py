from __future__ import annotations

from typing import Sequence


def to_ll(lst: Sequence[int]) -> dict[int, int]:
    return {lst[i]: lst[(i + 1) % len(lst)] for i in range(len(lst))}


def moves(ll: dict[int, int], current: int, iterations: int) -> int:
    minv = min(ll)
    maxv = max(ll)

    for _ in range(iterations):
        a = ll[current]
        b = ll[a]
        c = ll[b]
        ll[current] = ll[c]  # remove three elements
        dest = current - 1
        if dest < minv:
            dest = maxv
        while dest in (a, b, c):
            dest -= 1
            if dest < minv:
                dest = maxv
        ll[c] = ll[dest]
        ll[dest] = a  # insert the three
        current = ll[current]

    return current


if __name__ == "__main__":
    ilist = list(map(int, input("> ")))
    start = ilist[0]

    llist = to_ll(ilist)
    moves(llist, start, 100)
    o = llist[1]
    for i in range(8):
        print(o, end="")
        o = llist[o]
    print()

    llist = to_ll(ilist + list(range(10, 1000000 + 1)))
    moves(llist, start, 10000000)
    a = llist[1]
    b = llist[a]
    print(a * b)
