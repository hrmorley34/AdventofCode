from collections import Counter
from itertools import cycle
import random
from typing import NamedTuple


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


class DiracDie(Die):
    sides: int
    roll_count: int
    prepared: list[int]

    def __init__(self, sides: int) -> None:
        self.sides = sides
        self.roll_count = 0
        self.prepared = []

    def roll(self) -> int:
        self.roll_count += 1
        return self.prepared.pop(0)

    def copy(self) -> "DiracDie":
        copy = DiracDie(self.sides)
        copy.roll_count = self.roll_count
        copy.prepared = self.prepared.copy()
        return copy

    def fork(self, n: int = 3) -> list["DiracDie"]:
        values = list(range(1, self.sides + 1))
        dice: list[DiracDie] = [self]
        old_dice = dice
        for _ in range(n):
            dice = []
            for die in old_dice:
                for v in values:
                    copy = die.copy()
                    copy.prepared.append(v)
                    dice.append(copy)
            old_dice = dice
        return dice


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


class SimplePlayer(NamedTuple):
    square: int
    score: int


class Universes:
    # Counter[ Player1, Player2 ]
    board: Counter[tuple[SimplePlayer, SimplePlayer]]
    # tuple[ Player1 wins, Player2 wins ]
    wins: tuple[int, int]

    die: DiracDie
    board_size: int
    win_score: int

    def __init__(
        self, initial1: int, initial2: int, board_size: int = 10, win_score: int = 21
    ) -> None:
        self.board = Counter()
        self.board[SimplePlayer(initial1, 0), SimplePlayer(initial2, 0)] = 1
        self.wins = (0, 0)
        self.die = DiracDie(3)
        self.board_size = board_size
        self.win_score = win_score

    def go(self, player: SimplePlayer, die: Die) -> SimplePlayer:
        roll = sum(die.roll() for _ in range(3))
        square = (player.square + roll - 1) % self.board_size + 1
        return SimplePlayer(square, player.score + square)

    def next(self):
        newboard: Counter[tuple[SimplePlayer, SimplePlayer]] = Counter()
        newwin1, newwin2 = self.wins

        for (player1, player2), count in self.board.items():
            for die in self.die.fork(3):
                newplayer1 = self.go(player1, die)
                if newplayer1.score >= self.win_score:
                    newwin1 += count
                else:
                    for die in self.die.fork(3):
                        newplayer2 = self.go(player2, die)
                        if newplayer2.score >= self.win_score:
                            newwin2 += count
                        else:
                            newboard[newplayer1, newplayer2] += count

        self.board = newboard
        self.wins = newwin1, newwin2


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

    universes = Universes(P1START, P2START, board_size=BOARDSIZE, win_score=21)
    while universes.board:
        universes.next()
    print("Part 2:", max(universes.wins))
