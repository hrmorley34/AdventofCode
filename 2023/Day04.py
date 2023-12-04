from collections import Counter
from dataclasses import dataclass

from puzzle_input import puzzle_input


@dataclass
class Card:
    number: int
    winners: list[int]
    have: list[int]

    @classmethod
    def from_line(cls, line: str):
        numdef, parts = line.split(": ")
        win, have = parts.split(" | ")
        return cls(
            number=int(numdef[len("Card ") :]),
            winners=list(map(int, win.split())),
            have=list(map(int, have.split())),
        )

    def part1(self) -> int:
        common = len(set(self.winners) & set(self.have))
        return (1 << (common - 1)) if common else 0

    def part2(self, extra: Counter[int]) -> int:
        common = len(set(self.winners) & set(self.have))
        extra[self.number] += 1
        for i in range(self.number + 1, self.number + common + 1):
            extra[i] += extra[self.number]
        return extra[self.number]


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()
    cards = list(map(Card.from_line, PUZZLE_INPUT))

    print("Part 1:", sum(c.part1() for c in cards))
    copies: Counter[int] = Counter()
    print("Part 2:", sum(c.part2(copies) for c in cards))
