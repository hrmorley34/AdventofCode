from puzzle_input import puzzle_input


class GalaxyMap:
    map: list[str]

    galaxies: list[tuple[int, int]]
    empty_rows: list[int]
    empty_cols: list[int]

    def __init__(self, map: list[str]) -> None:
        self.map = map

        self.galaxies = self.find_galaxies()
        self.empty_rows = self.find_empty_rows()
        self.empty_cols = self.find_empty_columns()

    def find_empty_rows(self) -> list[int]:
        return [i for i, r in enumerate(self.map) if r.strip(".") == ""]

    def find_empty_columns(self) -> list[int]:
        return [
            i for i in range(len(self.map[0])) if all(row[i] == "." for row in self.map)
        ]

    def find_galaxies(self) -> list[tuple[int, int]]:
        return [
            (x, y)
            for y, row in enumerate(self.map)
            for x, cell in enumerate(row)
            if cell == "#"
        ]

    def find_distance(
        self, point1: tuple[int, int], point2: tuple[int, int], spread: int = 2
    ) -> int:
        dist = abs(point2[0] - point1[0]) + abs(point2[1] - point1[1])
        for y in range(min(point1[1], point2[1]) + 1, max(point1[1], point2[1])):
            if y in self.empty_rows:
                dist += spread - 1
        for x in range(min(point1[0], point2[0]) + 1, max(point1[0], point2[0])):
            if x in self.empty_cols:
                dist += spread - 1
        return dist

    def sum_distances(self, spread: int = 2) -> int:
        return sum(
            self.find_distance(self.galaxies[i], self.galaxies[j], spread)
            for i in range(len(self.galaxies) - 1)
            for j in range(i + 1, len(self.galaxies))
        )


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    galaxies = GalaxyMap(PUZZLE_INPUT)

    print("Part 1:", galaxies.sum_distances())
    print("Part 2:", galaxies.sum_distances(1_000_000))
