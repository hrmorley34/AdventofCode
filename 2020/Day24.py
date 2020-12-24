from puzzle_input import puzzle_input
from collections import defaultdict
import colorama
import re


RE_DIRECTIONS = re.compile("([ns]?[ew])")
DIRECTIONS = {"ne": 1 + 1j, "e": 2, "se": 1 - 1j, "sw": -1 - 1j, "w": -2, "nw": -1 + 1j}


class HexagonalGrid:
    cells: dict[complex, bool]  # True: black, False: white

    def __init__(self, default=False):
        self.cells = defaultdict(lambda: False)

    def __repr__(self):
        t = type(self).__name__ + "(\n"
        for y in range(10, -11, -1):
            for x in range(-20, 21):
                t += "O" if self.cells.get(x + y * 1j) else " "
            t += "\n"
        return t + ")"

    def flip_cell(self, path: str, start: complex = 0j):
        for d in RE_DIRECTIONS.finditer(path):
            start += DIRECTIONS[d[1].lower()]
        self.cells[start] ^= True

    def _update_should_flip(self, cell):
        total = sum(self.cells.get(cell + n, False) for n in DIRECTIONS.values())
        if self.cells.get(cell) and (total == 0 or total > 2):
            return True
        if not self.cells.get(cell) and (total == 2):
            return True
        return False

    def update(
        self, print_changes=False, print_changes_colour=True, xrange=30, yrange=15
    ):
        to_flip = set()
        for cell in list(self.cells):
            if self._update_should_flip(cell):
                to_flip.add(cell)
            for neighbour in DIRECTIONS.values():
                if self._update_should_flip(cell + neighbour):
                    to_flip.add(cell + neighbour)

        if print_changes:
            prevstate = self.cells.copy()

        for cell in to_flip:
            self.cells[cell] ^= True

        if print_changes:
            t = ""
            if print_changes_colour:
                ifred = colorama.Fore.RED
                ifgreen = colorama.Fore.LIGHTGREEN_EX
                ifreset = colorama.Fore.RESET
            else:
                ifred = ifgreen = ifreset = ""
            poststate = self.cells.copy()
            for y in range(yrange, -yrange - 1, -1):
                for x in range(-xrange, xrange + 1):
                    prev, post = prevstate.get(x + y * 1j), poststate.get(x + y * 1j)
                    if prev and post:
                        t += "O"
                    elif prev and not post:
                        t += ifred + "O" + ifreset
                    elif not prev and post:
                        t += ifgreen + "O" + ifreset
                    else:
                        t += " "
                t += "\n"
            print(t)


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    # PUZZLE_INPUT = input().splitlines()  # for hydrogen

    hg = HexagonalGrid()
    for path in PUZZLE_INPUT:
        hg.flip_cell(path)

    print(sum(hg.cells.values()))
    for x in range(100):
        hg.update()
    print(sum(hg.cells.values()))
