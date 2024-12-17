import re
from typing import Self

from puzzle_input import puzzle_input

RE_INIT = re.compile(
    r"Register A: (\d+)\n"
    r"Register B: (\d+)\n"
    r"Register C: (\d+)\n\n"
    r"Program: (\d+(?:,\d+)*)",
    re.MULTILINE,
)


class Computer:
    a: int
    b: int
    c: int

    script: list[int]
    pc: int

    output: list[int]

    def __init__(self, a: int, b: int, c: int, script: list[int]) -> None:
        self.a, self.b, self.c = a, b, c
        self.script = script
        self.pc = 0
        self.output = []

    def get_combo_value(self, operand: int) -> int:
        if 0 <= operand <= 3:
            return operand
        elif operand == 4:
            return self.a
        elif operand == 5:
            return self.b
        elif operand == 6:
            return self.c
        else:
            raise NotImplementedError

    def step(self) -> None:
        opcode, operand = self.script[self.pc : self.pc + 2]
        self.pc += 2
        match opcode:
            case 0:  # adv x => a = a / 2**combo(x)
                self.a >>= self.get_combo_value(operand)
            case 1:  # bxl x => b = b ^ x
                self.b ^= operand
            case 2:  # bst x => b = combo(x) % 8
                self.b = self.get_combo_value(operand) & 0b111
            case 3:  # jnz x : a? => pc = x
                if self.a != 0:
                    self.pc = operand
            case 4:  # bxc b c => b = b ^ c
                self.b = self.b ^ self.c
            case 5:  # out x => output.append(combo(x))
                self.output.append(self.get_combo_value(operand) & 0b111)
            case 6:  # bdv x => a = a / 2**combo(x)
                self.b = self.a >> self.get_combo_value(operand)
            case 7:  # acv x => a = a / 2**combo(x)
                self.c = self.a >> self.get_combo_value(operand)
            case _:
                raise NotImplementedError

    def run(self) -> list[int]:
        while self.pc < len(self.script):
            self.step()
        return self.output

    @classmethod
    def from_string(cls, s: str) -> Self:
        m = RE_INIT.fullmatch(s.strip())
        assert m is not None
        return cls(
            a=int(m[1]),
            b=int(m[2]),
            c=int(m[3]),
            script=list(map(int, m[4].split(","))),
        )


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input()
    COMPUTER = Computer.from_string(PUZZLE_INPUT)
    print("Part 1:", ",".join(map(str, COMPUTER.run())))
