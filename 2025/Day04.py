from dataclasses import dataclass

from puzzle_input import puzzle_input


@dataclass
class Grid:
    grid: list[list[str]]

    def neighbours(self, x: int, y: int) -> list[str]:
        neighbours: list[str] = []
        for dy in range(-1, 2):
            cy = y + dy
            if cy < 0:
                continue
            if cy >= len(self.grid):
                break
            for dx in range(-1, 2):
                if dx == 0 and dy == 0:
                    continue
                cx = x + dx
                if cx < 0:
                    continue
                if cx >= len(self.grid[cy]):
                    break
                neighbours.append(self.grid[cy][cx])
        return neighbours

    def find_accessible(self) -> list[tuple[int, int]]:
        accessible: list[tuple[int, int]] = []
        for y, line in enumerate(self.grid):
            for x, cell in enumerate(line):
                if cell != "@":
                    continue
                if self.neighbours(x, y).count("@") < 4:
                    accessible.append((x, y))
        return accessible

    def find_total_accessible(self) -> int:
        count = 0
        while True:
            acc = self.find_accessible()
            if not acc:
                break
            count += len(acc)
            for x, y in acc:
                self.grid[y][x] = "."  # remove roll
        return count


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    grid = Grid(list(map(list, PUZZLE_INPUT)))
    print("Part 1:", len(grid.find_accessible()))
    print("Part 2:", grid.find_total_accessible())
