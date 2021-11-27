from __future__ import annotations

from Day09 import IntcodeComputer, Memory
from typing import cast


NAT_ID = 255


# PART 1
class NotAlwaysTransmitting:
    memory: Memory
    queue: tuple[list[int], list[int]]

    def __init__(self):
        self.memory = Memory([])
        self.queue = ([], [])

    def run_until_input(self):
        pass

    def double_y(self) -> bool:
        return len(self.queue[1]) > 3 and self.queue[1][-1] == self.queue[1][-3]


class Network:
    computers: dict[int, IntcodeComputer | NotAlwaysTransmitting]
    idle: bool
    last_nat_y: int = -1

    def __init__(
        self,
        code: list[int],
        count: int,
        hardware: type[IntcodeComputer] = IntcodeComputer,
    ):
        self.computers = {i: hardware(code) for i in range(count)}
        for i, computer in self.computers.items():
            computer.queue[0].append(i)
            computer.run_until_input()
        self.computers[NAT_ID] = NotAlwaysTransmitting()
        self.idle = False

    def run_once(self):
        triggered = False
        for computer in self.computers.values():
            if isinstance(computer, NotAlwaysTransmitting):
                continue

            if len(computer.queue[0]) <= 0:
                computer.queue[0].append(-1)
            computer.run_until_input()
            while len(computer.queue[1]) >= 3:
                c = computer.queue[1].pop(0)
                x = computer.queue[1].pop(0)
                y = computer.queue[1].pop(0)
                self.computers[c].queue[0].extend([x, y])
                triggered = True
        self.idle = not triggered
        if self.idle:
            nat = cast(NotAlwaysTransmitting, self.computers[255])
            x, y = nat.queue[0][-2:]
            nat.queue[1].extend([x, y])
            self.computers[0].queue[0].extend([x, y])

    def run_until_nat(self) -> list[int]:
        nat = cast(NotAlwaysTransmitting, self.computers[NAT_ID])
        iterations = 0
        while len(nat.queue[0]) <= 0:
            self.run_once()
            iterations += 1
            if iterations % 20000 == 0:
                print(":", iterations)
        return nat.queue[0]

    def run_until_double_y(self) -> int:
        nat = cast(NotAlwaysTransmitting, self.computers[NAT_ID])
        iterations = 0
        while not nat.double_y():
            self.run_once()
            iterations += 1
            if iterations % 20000 == 0:
                print(":", iterations)
        return nat.queue[1][-1]


if __name__ == "__main__":
    PUZZLE_INPUT = [int(i) for i in input("> ").split(",")]

    n = Network(PUZZLE_INPUT, 50)
    v = n.run_until_nat()[1]
    print("Part 1:", v)

    v2 = n.run_until_double_y()
    print("Part 2:", v2)
