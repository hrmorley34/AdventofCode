from puzzle_input import puzzle_input


class LiteBingoBoard:
    to_find: list[set[int]]

    def __init__(self, board: list[list[int]]) -> None:
        assert len(board) == len(board[0])
        SIZE = len(board)
        rowf: list[set[int]] = list()
        colf: list[set[int]] = [set() for _ in range(SIZE)]
        for row in board:
            rowf.append(set(row))
            for coli, e in enumerate(row):
                colf[coli].add(e)

        self.to_find = list()
        self.to_find.extend(rowf)
        self.to_find.extend(colf)
        # self.to_find.extend(diags)

    def mark(self, number: int):
        for s in self.to_find:
            s.discard(number)

    @property
    def won(self) -> bool:
        # return True if any sets are empty
        return not all(self.to_find)

    @property
    def unmarked(self) -> set[int]:
        unmarked: set[int] = set()
        unmarked.update(*self.to_find)
        return unmarked


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    order, *boards = PUZZLE_INPUT.split("\n\n")
    ORDER = [int(i) for i in order.split(",")]
    BOARDS = [
        LiteBingoBoard([[int(i) for i in row.split()] for row in board.splitlines()])
        for board in boards
    ]

    orderit = iter(ORDER)
    call: int = -1
    markorder: list[tuple[int, LiteBingoBoard]] = []
    while BOARDS:
        call = next(orderit)
        for b in BOARDS:
            b.mark(call)

        for b in BOARDS.copy():
            if b.won:
                markorder.append((call, b))
                BOARDS.remove(b)

    print("Part 1:", sum(markorder[0][1].unmarked) * markorder[0][0])
    print("Part 2:", sum(markorder[-1][1].unmarked) * markorder[-1][0])
