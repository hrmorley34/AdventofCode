from itertools import cycle
import random


class Die:
    sides: int
    roll_count: int

    def __init__(self, sides: int) -> None:
        self.sides = sides
        self.roll_count = 0

    def roll(self) -> int:
        self.roll_count += 1
        return random.randint(1, self.sides)


class DeterministicDie(Die):
    last_roll: int

    def __init__(self, sides: int) -> None:
        super().__init__(sides)
        self.last_roll = 0

    def roll(self) -> int:
        self.roll_count += 1
        roll = self.last_roll % self.sides + 1
        self.last_roll = roll
        return roll


class Player:
    space: int
    score: int
    boardsize: int

    def __init__(self, space: int, boardsize: int) -> None:
        self.space = space
        self.score = 0
        self.boardsize = boardsize

    def move(self, die: Die) -> None:
        move = sum(die.roll() for _ in range(3))
        self.space = (self.space + move - 1) % self.boardsize + 1
        self.score += self.space


if __name__ == "__main__":
    P1START = int(input().split(": ", 1)[1])
    P2START = int(input().split(": ", 1)[1])
    BOARDSIZE = 10
    playeriter: "cycle[Player]" = cycle(
        (Player(P1START, BOARDSIZE), Player(P2START, BOARDSIZE))
    )

    DIE = DeterministicDie(100)
    for player in playeriter:
        player.move(DIE)
        if player.score >= 1000:
            break
    loser = next(playeriter)
    print("Part 1:", loser.score * DIE.roll_count)
