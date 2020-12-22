from collections import deque
from puzzle_input import puzzle_input


def run_round(deq1: deque, deq2: deque):
    c1 = deq1.popleft()
    c2 = deq2.popleft()
    if c1 == c2:
        raise ValueError("{} and {} are equal".format(c1, c2))
    elif c1 > c2:
        deq1.extend((c1, c2))
    else:
        deq2.extend((c2, c1))


def run_game_p2(deq1: deque, deq2: deque) -> bool:
    " True if p1; False if p2 "

    seens: set[tuple[tuple[int], tuple[int]]] = set()
    while len(deq1) and len(deq2):

        tform = (tuple(deq1), tuple(deq2))
        if tform in seens:
            return True
        else:
            seens.add(tform)

        c1 = deq1.popleft()
        c2 = deq2.popleft()
        if len(deq1) >= c1 and len(deq2) >= c2:
            winner = run_game_p2(
                deque(list(deq1)[:c1]), deque(list(deq2)[:c2])
            )
        else:
            winner = c1 > c2

        if winner:
            deq1.extend((c1, c2))
        else:
            deq2.extend((c2, c1))

    return bool(len(deq1))


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().split("\n\n")
    # PUZZLE_INPUT = "\n".join(input().splitlines()).split("\n\n")  # for hydrogen

    p1deque = deque(map(int, PUZZLE_INPUT[0].splitlines()[1:]))
    p2deque = deque(map(int, PUZZLE_INPUT[1].splitlines()[1:]))

    count = 0
    while len(p1deque) and len(p2deque):
        count += 1
        run_round(p1deque, p2deque)
    if p1deque:
        windeq = p1deque
    else:
        windeq = p2deque
    print(sum([i * v for i, v in enumerate(reversed(windeq), 1)]))

    # Part 2
    p1deque = deque(map(int, PUZZLE_INPUT[0].splitlines()[1:]))
    p2deque = deque(map(int, PUZZLE_INPUT[1].splitlines()[1:]))

    win = run_game_p2(p1deque, p2deque)
    windeq = p1deque if win else p2deque
    print(sum([i * v for i, v in enumerate(reversed(windeq), 1)]))
