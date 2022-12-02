from puzzle_input import puzzle_input


MOVES = {"A": 1, "B": 2, "C": 3, "X": 1, "Y": 2, "Z": 3}


def score_move(opponent: str, own: str) -> int:
    m1, m2 = MOVES[opponent], MOVES[own]
    if m1 == m2:
        return 3 + m2
    elif (m1 % 3) == (m2 - 1):
        return 6 + m2
    else:
        return m2


def score_winstate(opponent: str, winstate: str) -> int:
    m1 = MOVES[opponent]
    if winstate == "X":
        return ((m1 - 2) % 3) + 1
    elif winstate == "Y":
        return 3 + m1
    else:
        return 6 + (m1 % 3) + 1


if __name__ == "__main__":
    PUZZLE_INPUT = [line.split() for line in puzzle_input().splitlines()]

    part1 = sum(score_move(a, b) for a, b in PUZZLE_INPUT)
    print(f"Part 1: {part1}")
    part2 = sum(score_winstate(a, b) for a, b in PUZZLE_INPUT)
    print(f"Part 2: {part2}")
