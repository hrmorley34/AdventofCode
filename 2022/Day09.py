from puzzle_input import puzzle_input


def get_shift(direction: str) -> complex:
    if direction == "R":
        return 1 + 0j
    elif direction == "U":
        return 1j
    elif direction == "L":
        return -1
    elif direction == "D":
        return -1j
    else:
        assert False


def get_tail_shift(head: complex, tail: complex) -> complex:
    diff = head - tail
    if abs(diff.real) <= 1 and abs(diff.imag) <= 1:
        return 0j
    elif (diff.real == 0.0) or (diff.imag == 0.0):
        return diff / abs(diff)
    else:
        return diff.real / abs(diff.real) + diff.imag / abs(diff.imag) * 1j


def parse_step(step: str) -> tuple[str, int]:
    d, i = step.split()
    return d, int(i)


class HeadTail:
    head: complex
    tail: complex
    tail_visited: set[complex]

    def __init__(self, head: complex = 0j, tail: complex = 0j) -> None:
        self.head = head
        self.tail = tail
        self.tail_visited = {tail}

    def run_step(self, direction: str, value: int) -> None:
        d = get_shift(direction)
        for _ in range(value):
            self.head += d
            self.tail += get_tail_shift(self.head, self.tail)
            self.tail_visited.add(self.tail)


class MultiHeadTail:
    head: complex
    tails: list[complex]
    tail_visited: set[complex]

    def __init__(self, tails: int = 9) -> None:
        self.head = 0j
        self.tails = [0j for _ in range(tails)]
        self.tail_visited = {0j}

    def run_step(self, direction: str, value: int) -> None:
        d = get_shift(direction)
        for _ in range(value):
            self.head += d
            for i in range(len(self.tails)):
                local_head = self.head if i <= 0 else self.tails[i - 1]
                self.tails[i] += get_tail_shift(local_head, self.tails[i])
            self.tail_visited.add(self.tails[-1])


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    moves = list(map(parse_step, PUZZLE_INPUT))

    h = HeadTail()
    h2 = MultiHeadTail()
    for d, i in moves:
        h.run_step(d, i)
        h2.run_step(d, i)
    print(f"Part 1: {len(h.tail_visited)}")
    print(f"Part 2: {len(h2.tail_visited)}")
