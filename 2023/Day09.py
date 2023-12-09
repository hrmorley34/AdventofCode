from puzzle_input import puzzle_input


def deriv(seq: list[int]) -> list[int]:
    return [b - a for a, b in zip(seq, seq[1:])]


def extrapolate(seq: list[int], backwards: bool = False) -> int:
    lasts: list[int] = []
    while any(seq):  # any non-zero value
        lasts.append(seq[0 if backwards else -1])
        seq = deriv(seq)
    d: int = 0
    for i in reversed(lasts):
        # next num in sequence is prev + below
        d = i + (-d if backwards else d)
    return d


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    histories = [list(map(int, line.split())) for line in PUZZLE_INPUT]

    print("Part 1:", sum(extrapolate(s, False) for s in histories))
    print("Part 2:", sum(extrapolate(s, True) for s in histories))
