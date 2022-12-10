from puzzle_input import puzzle_input


class CPU:
    cycle: int
    cycle20sum: int
    screen: list[list[bool]]
    x: int

    def __init__(self) -> None:
        self.cycle = 0
        self.cycle20sum = 0
        self.screen = []
        self.x = 1

    def inc_cycle(self) -> None:
        if self.cycle % 40 == 0:
            self.screen.append([])
        self.cycle += 1
        if self.cycle % 40 == 20:
            self.cycle20sum += self.x * self.cycle

        dx = abs(self.x - ((self.cycle - 1) % 40))
        self.screen[-1].append(dx <= 1)

    def run_instruction(self, instruction: str) -> None:
        match instruction.split():
            case ["noop"]:
                self.inc_cycle()
            case ["addx", v]:
                self.inc_cycle()
                self.inc_cycle()
                self.x += int(v)
            case _:
                assert False, instruction


if __name__ == "__main__":
    PUZZLE_INPUT = puzzle_input().splitlines()

    c = CPU()
    for instr in PUZZLE_INPUT:
        c.run_instruction(instr)
    print(f"Part 1: {c.cycle20sum}")
    print("Part 2:")
    print("\n".join("".join("#" if b else "." for b in line) for line in c.screen))
