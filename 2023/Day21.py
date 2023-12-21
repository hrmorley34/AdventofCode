from dataclasses import dataclass

from puzzle_input import puzzle_input

ADJ = [(-1, 0), (0, -1), (1, 0), (0, 1)]


@dataclass
class Map:
    blocks: set[tuple[int, int]]
    dim: tuple[int, int]
    pos: set[tuple[int, int]]

    @classmethod
    def from_map(cls, grid: list[str]):
        blocks: set[tuple[int, int]] = set()
        initial: set[tuple[int, int]] = set()
        for y, line in enumerate(grid):
            for x, c in enumerate(line):
                if c == "#":
                    blocks.add((x, y))
                elif c == "S":
                    initial.add((x, y))
                else:
                    assert c == "."
        return cls(blocks, (len(grid[0]), len(grid)), initial)

    def step(self) -> None:
        new_pos: set[tuple[int, int]] = set()
        for px, py in self.pos:
            for dx, dy in ADJ:
                x, y = px + dx, py + dy
                if (x, y) in self.blocks:
                    continue
                elif x < 0 or x >= self.dim[0] or y < 0 or y >= self.dim[1]:
                    print("WARNING: Out of range")
                    continue
                else:
                    new_pos.add((x, y))
        self.pos = new_pos


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    m = Map.from_map(PUZZLE_INPUT)

    for _ in range(64):
        m.step()
    print("Part 1:", len(m.pos))
