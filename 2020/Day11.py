from puzzle_input import puzzle_input

ADJACENT = (1, 1 + 1j, 1j, -1 + 1j, -1, -1 - 1j, -1j, 1 - 1j)


class SeatingSystem:
    board: list[str]

    def __init__(self, board: list[str]):
        if isinstance(board, str):
            board = board.splitlines()
        self.board = board

    def __repr__(self):
        return "Board(\n  {}\n)".format("\n  ".join(self.board))

    def __hash__(self):
        return hash(tuple(self.board))

    def get(self, xy, default=None):
        if isinstance(xy, complex):
            x, y = int(xy.real), int(xy.imag)
        else:
            x, y = xy
        if 0 <= y < len(self.board) and 0 <= x < len(self.board[y]):
            return self.board[y][x]
        else:
            return default

    def look_direction(self, pos: complex, direction: complex):
        pos += direction
        while self.get(pos) == ".":
            pos += direction
        return self.get(pos)

    def update(self, lookmode: bool = False, neighbourtol: int = 4):
        old_board = list(self.board)
        new_board = []
        for y, row in enumerate(old_board):
            new_board.append("")
            for x, c in enumerate(row):
                if c == ".":
                    new_board[-1] += "."
                    continue
                if lookmode:
                    neighbours = [
                        self.look_direction(complex(x, y), adj) for adj in ADJACENT
                    ]
                else:
                    neighbours = [self.get(complex(x, y) + adj) for adj in ADJACENT]
                neighbourcount = neighbours.count("#")
                if c == "L":
                    if neighbourcount <= 0:
                        new_board[-1] += "#"
                    else:
                        new_board[-1] += "L"
                elif c == "#":
                    if neighbourcount < neighbourtol:
                        new_board[-1] += "#"
                    else:
                        new_board[-1] += "L"
                else:
                    print(x, y, c)
        self.board = new_board


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    seats = SeatingSystem(PUZZLE_INPUT)
    states = set()
    while seats not in states:
        states.add(seats)
        seats.update()

    print(sum(r.count("#") for r in seats.board))

    seats = SeatingSystem(PUZZLE_INPUT)
    states = set()
    while seats not in states:
        states.add(seats)
        seats.update(lookmode=True, neighbourtol=5)

    print(sum(r.count("#") for r in seats.board))
