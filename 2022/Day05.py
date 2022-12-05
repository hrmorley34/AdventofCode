import re
from itertools import count
from puzzle_input import puzzle_input


STATE = list[list[str]]


def move(state: STATE, count: int, src: int, dest: int, *, c9001: bool = False) -> None:
    if c9001:
        appends: list[str] = []
        for _ in range(count):
            appends.insert(0, state[src].pop(-1))
        state[dest].extend(appends)
    else:
        for _ in range(count):
            state[dest].append(state[src].pop(-1))


MOVE_LINE_RE = re.compile(r"^move (\d+) from (\d+) to (\d+)$")


def parse_move_line(line: str) -> tuple[int, int, int]:
    m = MOVE_LINE_RE.match(line)
    assert m
    c, s, d = m.groups()
    return int(c), int(s), int(d)


def parse_state(text: str) -> STATE:
    state: STATE = [[]]  # no index 0
    textlines = text.splitlines()
    for i, ri in zip(count(1), map(int, textlines[-1][1::4])):
        assert i == ri
        state.append([])
    for line in textlines[-2::-1]:
        for index, crate in zip(count(1), line[1::4]):
            if crate == " ":
                continue
            state[index].append(crate)
    return state


def print_state(state: STATE) -> None:
    max_index = max(map(len, state)) - 1
    for index in range(max_index, -1, -1):
        print(
            " ".join(
                f"[{stack[index]}]" if len(stack) > index else "   "
                for stack in state[1:]
            )
        )
    print(" ".join(f" {i} " for i, _ in zip(count(1), state[1:])))


def tops(state: STATE) -> str:
    tps = ""
    for stack in state[1:]:
        tps += stack[-1]
    return tps


if __name__ == "__main__":
    PUZZLE_INPUT = "\n".join(puzzle_input().splitlines())
    INITIAL_STATE_INPUT, MOVES_INPUT = PUZZLE_INPUT.split("\n\n")

    state1 = parse_state(INITIAL_STATE_INPUT)
    state2 = parse_state(INITIAL_STATE_INPUT)

    for mv in map(parse_move_line, MOVES_INPUT.splitlines()):
        move(state1, *mv)
        move(state2, *mv, c9001=True)

    print(f"Part 1: {tops(state1)}")
    print(f"Part 2: {tops(state2)}")
